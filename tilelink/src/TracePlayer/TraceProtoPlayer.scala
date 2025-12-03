package verif.etrace

import com.verif.TraceProtos._
import com.verif.TraceProtos.InstDepRecord.RecordType._

import java.util.zip.GZIPInputStream
import com.google.protobuf.CodedInputStream

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import java.io.{FileWriter, BufferedWriter}
import java.io.{File, FileInputStream}

sealed trait NodeType
case object COMP extends NodeType
case object LOAD extends NodeType
case object STORE extends NodeType

sealed trait NodeStatus
case object NotReady extends NodeStatus
case class Executing(remaining: Long) extends NodeStatus  // For COMP or pre-issue LOAD/STORE
case object WaitingForIssue extends NodeStatus            // When a L/S is issued to cache
case object WaitingForAck extends NodeStatus              // When a Load/Store is acked by the cache as complete
case object Completed extends NodeStatus



case class TraceNode(
  seqNum: Long,
  nodeType: NodeType,
  compDelay: Long,
  robDeps: List[Long] = Nil,
  regDeps: List[Long] = Nil,
  pc: Long,
  pAddr: Option[Long] = None,
  size: Option[Int] = None,
  flags: Option[Int] = None
)

case class MemReqTime(
  seqNum: Long,
  nodeType: String,
  reqTime: Long
)

class ElasticTraceDAG(traceFileName: String, numTraces: Int) {
  private val nodes = mutable.Map[Long, TraceNode]()
  private val dependencies = mutable.Map[Long, Set[Long]]()
  private val completed = mutable.Set[Long]()
  private val nodeStatus = mutable.Map[Long, NodeStatus]()
  // private val pendingLoads = mutable.LinkedHashMap[Long, TraceNode]()
  // private val pendingStores = mutable.LinkedHashMap[Long, TraceNode]()
  private val issuedLoads = mutable.LinkedHashMap[Long, TraceNode]()
  private val issuedStores = mutable.LinkedHashMap[Long, TraceNode]()
  private val pendingReqs = mutable.LinkedHashMap[Long, TraceNode]()
  private val memReqTimes = mutable.Map[Long, MemReqTime]()
  
  var clock = 0L
  var header: InstDepRecordHeader = _

  loadTrace()

  private def loadTrace(): Unit = {
    //println("hello world from trace reader!")
    val traceFile = new File(traceFileName)
    require(traceFile.exists(), s"Could not find trace file: ${traceFile.getAbsolutePath}")
    val stream = new FileInputStream(traceFile)
    // val stream = getClass.getResourceAsStream(traceFile)
    require(stream != null, s"Could not find trace file: $traceFile")

    val gzipStream = new GZIPInputStream(stream)
    val codedInput = CodedInputStream.newInstance(gzipStream)

    codedInput.readRawLittleEndian32() // Skip magic

    val headerSize = codedInput.readRawVarint32()
    val headerLimit = codedInput.pushLimit(headerSize)
    header = InstDepRecordHeader.parseFrom(codedInput)
    codedInput.popLimit(headerLimit)

    println(s"[Header] tickFreq=${header.getTickFreq}  windowSize=${header.getWindowSize}")

    var recordCount = 0
    while (!codedInput.isAtEnd && recordCount < numTraces) {
      val msgSize = codedInput.readRawVarint32()
      val limit = codedInput.pushLimit(msgSize)
      val record = InstDepRecord.parseFrom(codedInput)
      codedInput.popLimit(limit)

      if(recordCount == 0){ //get initial icache packets from each core
        //println(record)
      }

      addFromRecord(record)
      recordCount += 1
    }

    println(s"[Load] Parsed $recordCount data records")
    gzipStream.close()

    // After parsing all nodes
    nodes.keys.foreach { seq =>
      nodeStatus(seq) = NotReady
    }

  }

  private def addFromRecord(record: InstDepRecord): Unit = {
    val nodeType = record.getType match {
      case InstDepRecord.RecordType.COMP  => COMP
      case InstDepRecord.RecordType.LOAD  => LOAD
      case InstDepRecord.RecordType.STORE => STORE
      case other => throw new IllegalArgumentException(s"Unsupported type: $other")
    }

    val robDeps: List[Long] =
        if (record.getRobDepCount != 0)
            record.getRobDepList.asScala.map(_.longValue).toList
        else
            Nil
    val regDeps = record.getRegDepList.toArray.toList.map(_.toString.toLong)

    val node = TraceNode(
      seqNum    = record.getSeqNum,
      nodeType  = nodeType,
      compDelay = record.getCompDelay,
      robDeps   = robDeps,
      regDeps   = regDeps,
      pc        = record.getPc,
      pAddr     = if (record.hasPAddr) Some(record.getPAddr) else None,
      size      = if (record.hasSize) Some(record.getSize) else None,
      flags     = if (record.hasFlags) Some(record.getFlags) else None
    )

    nodes(node.seqNum) = node
    dependencies(node.seqNum) = (robDeps ++ regDeps).toSet
  }

  def step(): Unit = {
  clock += 1

  // 1. Mark nodes as Ready if all deps done
  for ((seq, node) <- nodes) {
    if (nodeStatus(seq) == NotReady && dependencies(seq).forall(completed.contains)) {
      val delay = node.compDelay max 1
      nodeStatus(seq) = Executing(delay)
      //println(s"[Cycle $clock] Executing node $seq")
    }
  }

  for ((seq, Executing(remaining)) <- nodeStatus.toList) {
    if (remaining <= 1) {
      val node = nodes(seq)
      node.nodeType match {
        case COMP =>
          nodeStatus(seq) = Completed
          completed += seq
          //println(s"[Cycle $clock] COMP $seq done")

        case LOAD =>
          pendingReqs += (seq -> node)
          nodeStatus(seq) = WaitingForIssue
          //println(s"[Cycle $clock] Issuing LOAD $seq")

        case STORE =>
          pendingReqs += (seq -> node)
          nodeStatus(seq) = WaitingForIssue
          //println(s"[Cycle $clock] Issuing STORE $seq")
      }
    } else {
      nodeStatus(seq) = Executing(remaining - 1000)
    }
  }

  // LOAD/STORE completion deferred to ack()
}
  def getPendingReq: Option[TraceNode] = pendingReqs.headOption.map(_._2)

  def getIssuedLoads: Iterator[TraceNode] = issuedLoads.valuesIterator
  def getIssuedStores: Iterator[TraceNode] = issuedStores.valuesIterator

  def issueLoad(seqNum: Long): Unit = {
    if (pendingReqs.contains(seqNum)) {
      //println(s"[Cycle $clock] LOAD issued: $seqNum")
      issuedLoads += (seqNum -> pendingReqs(seqNum))
      pendingReqs -= seqNum 
      nodeStatus(seqNum) = WaitingForAck
    }
  }

  def issueStore(seqNum: Long): Unit = {
    if (pendingReqs.contains(seqNum)) {
      //println(s"[Cycle $clock] STORE issued: $seqNum")
      issuedStores += (seqNum -> pendingReqs(seqNum))
      pendingReqs -= seqNum
      nodeStatus(seqNum) = WaitingForAck
    }
  }

  def acknowledgeLoad(seqNum: Long): Unit = {
    if (issuedLoads.contains(seqNum)) {
      //println(s"[Cycle $clock] LOAD $seqNum acked after ${memReqTimes(seqNum)} Cycles")
      // //println(seqNum)
      issuedLoads -= seqNum 
      completed += seqNum
      nodeStatus(seqNum) = Completed
    }
  }

  def acknowledgeStore(seqNum: Long): Unit = {
    if (issuedStores.contains(seqNum)) {
      //println(s"[Cycle $clock] STORE $seqNum acked after ${memReqTimes(seqNum)} Cycles")
      // //println(seqNum)
      issuedStores -= seqNum
      completed += seqNum
      nodeStatus(seqNum) = Completed
    }
  }

  def incrementLoadTime(seqNum: Long): Unit = {
    if (memReqTimes.contains(seqNum)){
      memReqTimes(seqNum)= MemReqTime(seqNum, "Load", memReqTimes(seqNum).reqTime + 1)
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def incrementStoreTime(seqNum: Long): Unit = {
    if (memReqTimes.contains(seqNum)){
      memReqTimes(seqNum)= MemReqTime(seqNum, "Store", memReqTimes(seqNum).reqTime + 1)
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Store", 1L)
    }
  }

  def isDone: Boolean = completed.size == nodes.size

  def debug(): Unit ={
    for ((seq, node) <- nodes) {
        if (nodeStatus(seq) == NotReady) {
          println(node)
          for (dep <- node.robDeps){
            if(!completed.contains(dep)){
              println(s"${dep} IS NOT ACKED")
              println(nodes(dep))
            }
          }
      }
    }
    for ((seq, node) <- nodes) {
      if (nodeStatus(seq) != Completed){
        println(s"NODE: ${seq}, STATUS: ${nodeStatus(seq)}")
      }
    }
  }

  def log(name: String, seqNum: Long): Unit = {
    MemReqLogger.log(name, memReqTimes(seqNum))
  }
}

case class InstNode(
  tick: Long,
  cmd: Int,
  addr: Int,
  size: Int,
  flags: Option[Int],
  pkt_id: Option[Long],
  pc: Option[Long]
)

class InstTraceDAG(traceFileName: String, numTraces: Int) {
  private val nodes = mutable.Map[Long, InstNode]()
  private val completed = mutable.Set[Long]()
  private val nodeStatus = mutable.Map[Long, NodeStatus]()
  private val issuedLoads = mutable.LinkedHashMap[Long, InstNode]()
  private val pendingReqs = mutable.LinkedHashMap[Long, InstNode]()
  private val memReqTimes = mutable.Map[Long, MemReqTime]()
  
  var clock = 0L
  var header: PacketHeader = _

  loadTrace()

  private def loadTrace(): Unit = {
    // println("hello world from i-trace reader!")
    val traceFile = new File(traceFileName)
    require(traceFile.exists(), s"Could not find trace file: ${traceFile.getAbsolutePath}")
    val stream = new FileInputStream(traceFile)
    require(stream != null, s"Could not find trace file: $traceFile")

    val gzipStream = new GZIPInputStream(stream)
    val codedInput = CodedInputStream.newInstance(gzipStream)

    codedInput.readRawLittleEndian32() // Skip magic

    val headerSize = codedInput.readRawVarint32()
    val headerLimit = codedInput.pushLimit(headerSize)
    header = PacketHeader.parseFrom(codedInput)
    codedInput.popLimit(headerLimit)

    println(s"[I-Header] tickFreq=${header.getTickFreq}")

    var recordCount = 0
    while (!codedInput.isAtEnd && recordCount < numTraces) {
      val msgSize = codedInput.readRawVarint32()
      val limit = codedInput.pushLimit(msgSize)
      val record = Packet.parseFrom(codedInput)
      if(recordCount == 0){ //get initial icache packets from each core
        //println(record)
      }
      codedInput.popLimit(limit)

      addFromRecord(record)
      recordCount += 1
    }

    println(s"[Load] Parsed $recordCount records")
    gzipStream.close()

    // After parsing all nodes
    nodes.keys.foreach { seq =>
      nodeStatus(seq) = NotReady
    }

  }

  private def addFromRecord(record: Packet): Unit = {
    val node = InstNode(
      tick    = record.getTick.toLong,
      cmd     = record.getCmd.toInt,
      addr    = record.getAddr.toInt,
      size    = record.getSize.toInt,
      flags   = if (record.hasFlags) Some(record.getFlags.toInt) else None,
      pkt_id  = if (record.hasPktId) Some(record.getPktId.toLong) else None,
      pc      = if (record.hasPc) Some(record.getPc.toLong) else None
    )

    nodes(node.tick) = node
  }

  def step(): Unit = {
  clock += 1

  // 1. Mark nodes as Ready if all deps done
  for ((seq, node) <- nodes) {
    if (nodeStatus(seq) == NotReady && clock*1000 >= seq) { //since tick == seq
      pendingReqs += (seq -> node)
      nodeStatus(seq) = WaitingForAck
      //println(s"[Cycle $clock] Executing node $seq")
    }
  }

  // LOAD/STORE completion deferred to ack()
  }

  def getPendingReq: Option[InstNode] = pendingReqs.headOption.map(_._2)

  def getIssuedLoads: Iterator[InstNode] = issuedLoads.valuesIterator

  def issueLoad(seqNum: Long): Unit = {
    if (pendingReqs.contains(seqNum)) {
      //println(s"[Cycle $clock] I-LOAD issued: $seqNum")
      issuedLoads += (seqNum -> pendingReqs(seqNum))
      pendingReqs -= seqNum 
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def acknowledgeLoad(seqNum: Long): Unit = {
    if (issuedLoads.contains(seqNum)) {
      //println(s"[Cycle $clock] I-LOAD $seqNum acked after ${memReqTimes(seqNum)} Cycles")
      // println(seqNum)
      issuedLoads -= seqNum 
      completed += seqNum
    }
  }

  def incrementLoadTime(seqNum: Long): Unit = {
    if (memReqTimes.contains(seqNum)){
      memReqTimes(seqNum)= MemReqTime(seqNum, "Load", memReqTimes(seqNum).reqTime + 1)
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def isDone: Boolean = completed.size == nodes.size

  def log(name: String, seqNum: Long): Unit = {
    MemReqLogger.log(name, memReqTimes(seqNum))
  }
}

object MemReqLogger {
  private val logFile = new BufferedWriter(new FileWriter("test_run_dir/mem_req_times/mem_req_time.csv", false))
  private var headerWritten = false
  println("Working directory: " + new java.io.File(".").getAbsolutePath)


  /** Write one entry to the CSV log */
  def log(dagID: String, memReqTime: MemReqTime): Unit = synchronized {
    if (!headerWritten) {
      logFile.write("dagID,blockID,nodeType,reqTime\n")
      headerWritten = true
    }
    logFile.write(s"$dagID,${memReqTime.seqNum},${memReqTime.nodeType},${memReqTime.reqTime}\n")
    logFile.flush()
  }

  def close(): Unit = logFile.close()
}