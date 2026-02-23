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

case class WatchDogTimeoutError() extends Exception() {}

class ElasticTraceDAG(traceFileName: String) {//, numTraces: Int) {
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

  private var stream: FileInputStream = _
  private var gzipStream: GZIPInputStream = _
  private var codedInput: CodedInputStream = _
  private var eof: Boolean = false

  private val LOW_WATER = 10    // when to load more
  private val BATCH_LOAD = 200   // how many to load per request
  private val MAX_NODES_IN_MEMORY = 10000
  private val WATCH_DOG_TIMEOUT = 10000 //max number of cycles a request could take?

  openStream()
  ensureEnoughReadyNodes()

  private def openStream(): Unit = {
    val traceFile = new File(traceFileName)
    require(traceFile.exists(), s"Could not find trace file: ${traceFile.getAbsolutePath}")

    stream = new FileInputStream(traceFile)
    gzipStream = new GZIPInputStream(stream)
    codedInput = CodedInputStream.newInstance(gzipStream)

    codedInput.readRawLittleEndian32() // magic

    val headerSize = codedInput.readRawVarint32()
    val headerLimit = codedInput.pushLimit(headerSize)
    header = InstDepRecordHeader.parseFrom(codedInput)
    codedInput.popLimit(headerLimit)

    println(s"[Header] tickFreq=${header.getTickFreq}  windowSize=${header.getWindowSize}")
  }

  private def loadNextBatch(max: Int = 2000): Int = {
    if (eof) return 0

    var count = 0
    try {
      while (count < max) {
        val msgSize = codedInput.readRawVarint32()   // may EOF here
        val limit = codedInput.pushLimit(msgSize)
        val record = InstDepRecord.parseFrom(codedInput)
        codedInput.popLimit(limit)

        addFromRecord(record)
        nodeStatus(record.getSeqNum) = NotReady
        count += 1
      }
    } catch {
      case _: java.io.EOFException |
          _: com.google.protobuf.InvalidProtocolBufferException =>
        eof = true
        println(s"[Stream] EOF after adding $count records.")
    }

    count
  }

  private def ensureEnoughReadyNodes(): Unit = {
    val readyCount = nodeStatus.count(_._2 == NotReady)
    if (!eof && readyCount < LOW_WATER) {
      val loaded = loadNextBatch(BATCH_LOAD)
      if (loaded > 0)
        println(s"[Stream] Loaded $loaded additional DATA trace nodes")
    }
  }

  private def pruneCompleted(): Unit = {
    if (nodes.size <= MAX_NODES_IN_MEMORY) return

    val excess = nodes.size - MAX_NODES_IN_MEMORY

    // prune in FIFO order of seqNum
    val sortedCompleted =
      completed.toSeq.sorted.take(excess)

    for (seq <- sortedCompleted) {
      nodes.remove(seq)
      dependencies.remove(seq)
      nodeStatus.remove(seq)
      completed.remove(seq)
    }

    println(s"[Stream] Pruned $excess completed DATA nodes (kept newest ${MAX_NODES_IN_MEMORY})")
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
    ensureEnoughReadyNodes()
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
  pruneCompleted()

  // println("data cache player memory sizes:")
  // println(s"nodes: ${nodes.size} dependencies: ${dependencies.size} completed: ${completed.size} nodeStatus: ${nodeStatus.size} issuedLoads: ${issuedLoads.size} issuedStores: ${issuedStores.size} pendingReqs: ${pendingReqs.size} memReqTimes: ${memReqTimes.size}")
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
      if (memReqTimes(seqNum).reqTime >=  WATCH_DOG_TIMEOUT) {
        println(s"Node timed out: ${memReqTimes(seqNum)}")
        debug()
        throw new WatchDogTimeoutError()
      }
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def incrementStoreTime(seqNum: Long): Unit = {
    if (memReqTimes.contains(seqNum)){
      memReqTimes(seqNum)= MemReqTime(seqNum, "Store", memReqTimes(seqNum).reqTime + 1)
      if (memReqTimes(seqNum).reqTime >=  WATCH_DOG_TIMEOUT) {
        println(s"Node timed out: ${memReqTimes(seqNum)}")
        debug()
        throw new WatchDogTimeoutError()
      }
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Store", 1L)
    }
  }

  def isDone: Boolean = (completed.size >= nodes.size) && eof

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
    memReqTimes.remove(seqNum)
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

class InstTraceDAG(traceFileName: String) {//, numTraces: Int) {
  private val nodes = mutable.Map[Long, InstNode]()
  private val completed = mutable.Set[Long]()
  private val nodeStatus = mutable.Map[Long, NodeStatus]()
  private val issuedLoads = mutable.LinkedHashMap[Long, InstNode]()
  private val pendingReqs = mutable.LinkedHashMap[Long, InstNode]()
  private val memReqTimes = mutable.Map[Long, MemReqTime]()
  
  var clock = 0L
  var header: PacketHeader = _

  private var stream: FileInputStream = _
  private var gzipStream: GZIPInputStream = _
  private var codedInput: CodedInputStream = _
  private var eof: Boolean = false

  private val LOW_WATER = 10    // when to load more
  private val BATCH_LOAD = 20   // how many to load per request
  private val MAX_NODES_IN_MEMORY = 100 //no deps, store way less (but still something for my sanity)
  private val WATCH_DOG_TIMEOUT = 10000

  openStream()
  ensureEnoughReadyNodes()

  private def openStream(): Unit = {
    val traceFile = new File(traceFileName)
    require(traceFile.exists(), s"Could not find trace file: ${traceFile.getAbsolutePath}")

    stream = new FileInputStream(traceFile)
    gzipStream = new GZIPInputStream(stream)
    codedInput = CodedInputStream.newInstance(gzipStream)

    codedInput.readRawLittleEndian32() // magic

    val headerSize = codedInput.readRawVarint32()
    val headerLimit = codedInput.pushLimit(headerSize)
    header = PacketHeader.parseFrom(codedInput)
    codedInput.popLimit(headerLimit)

    println(s"[Header] i-trace tickFreq=${header.getTickFreq}")
  }

  private def loadNextBatch(max: Int = 2000): Int = {
    if (eof) return 0

    var count = 0
    try {
      while (count < max) {
        val msgSize = codedInput.readRawVarint32()   // may EOF here
        val limit = codedInput.pushLimit(msgSize)
        val record = Packet.parseFrom(codedInput)
        codedInput.popLimit(limit)

        addFromRecord(record)
        nodeStatus(record.getTick) = NotReady
        count += 1
      }
    } catch {
      case _: java.io.EOFException |
          _: com.google.protobuf.InvalidProtocolBufferException =>
        eof = true
        println(s"[Stream] EOF after adding $count records.")
    }

    count
  }

  private def ensureEnoughReadyNodes(): Unit = {
    val readyCount = nodeStatus.count(_._2 == NotReady) + nodeStatus.count(_._2 == WaitingForIssue)
    if (!eof && readyCount < LOW_WATER) {
      val loaded = loadNextBatch(BATCH_LOAD)
      if (loaded > 0)
        println(s"[Stream] Loaded $loaded additional INST trace nodes")
    }
  }

  private def pruneCompleted(): Unit = {
    if (nodes.size <= MAX_NODES_IN_MEMORY) return

    val excess = nodes.size - MAX_NODES_IN_MEMORY

    // prune in FIFO order of seqNum
    val sortedCompleted =
      completed.toSeq.sorted.take(excess)

    if (sortedCompleted.size == 0) {
      println(s"[WARN] [Stream] No completed nodes with max in memory!!")
      debug()
    }

    for (seq <- sortedCompleted) {
      nodes.remove(seq)
      nodeStatus.remove(seq)
      completed.remove(seq)
    }

    println(s"[Stream] Pruned $excess completed INST nodes (kept newest ${MAX_NODES_IN_MEMORY})")
  }

  // private def loadTrace(): Unit = {
  //   // println("hello world from i-trace reader!")
  //   val traceFile = new File(traceFileName)
  //   require(traceFile.exists(), s"Could not find trace file: ${traceFile.getAbsolutePath}")
  //   val stream = new FileInputStream(traceFile)
  //   require(stream != null, s"Could not find trace file: $traceFile")

  //   val gzipStream = new GZIPInputStream(stream)
  //   val codedInput = CodedInputStream.newInstance(gzipStream)

  //   codedInput.readRawLittleEndian32() // Skip magic

  //   val headerSize = codedInput.readRawVarint32()
  //   val headerLimit = codedInput.pushLimit(headerSize)
  //   header = PacketHeader.parseFrom(codedInput)
  //   codedInput.popLimit(headerLimit)

  //   println(s"[I-Header] tickFreq=${header.getTickFreq}")

  //   var recordCount = 0
  //   val unlimited = (numTraces == 0)

  //   try {
  //     while (unlimited || recordCount < numTraces) {
  //       val msgSize =
  //         try {
  //           codedInput.readRawVarint32()
  //         } catch {
  //           case _: java.io.EOFException =>
  //             // normal end of gzip-file
  //             println(s"[Load] EOF reached after $recordCount records")
  //             return
  //         }

  //       val limit = codedInput.pushLimit(msgSize)
  //       val record =
  //         try {
  //           Packet.parseFrom(codedInput)
  //         } catch {
  //           case _: com.google.protobuf.InvalidProtocolBufferException =>
  //             // This also means EOF or truncated msg
  //             println(s"[Load] Stopping: Invalid or truncated protobuf after $recordCount records")
  //             return
  //         }

  //       codedInput.popLimit(limit)

  //       addFromRecord(record)
  //       recordCount += 1
  //     }
  //   } finally {
  //     println(s"[Load] Parsed $recordCount records")
  //     gzipStream.close()
  //   }

  //   // After parsing all nodes
  //   nodes.keys.foreach { seq =>
  //     nodeStatus(seq) = NotReady
  //   }

  // }

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
    ensureEnoughReadyNodes()
    clock += 1

    // 1. Mark nodes as Ready if all deps done
    for ((seq, node) <- nodes) {
      if (nodeStatus(seq) == NotReady && clock*1000 >= seq) { //since tick == seq
        pendingReqs += (seq -> node)
        nodeStatus(seq) = WaitingForIssue
        //println(s"[Cycle $clock] Executing node $seq")
      }
    }

    // LOAD/STORE completion deferred to ack()
    pruneCompleted()

    // println("inst cache player memory sizes:")
    // println(s"nodes: ${nodes.size} completed: ${completed.size} nodeStatus: ${nodeStatus.size} issuedLoads: ${issuedLoads.size} pendingReqs: ${pendingReqs.size} memReqTimes: ${memReqTimes.size}")
  }

  def getPendingReq: Option[InstNode] = pendingReqs.headOption.map(_._2)

  def getIssuedLoads: Iterator[InstNode] = issuedLoads.valuesIterator

  def issueLoad(seqNum: Long): Unit = {
    if (pendingReqs.contains(seqNum)) {
      //println(s"[Cycle $clock] I-LOAD issued: $seqNum")
      issuedLoads += (seqNum -> pendingReqs(seqNum))
      pendingReqs -= seqNum 
      nodeStatus(seqNum) = WaitingForAck
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def acknowledgeLoad(seqNum: Long): Unit = {
    if (issuedLoads.contains(seqNum)) {
      println(s"[Cycle $clock] I-LOAD $seqNum acked after ${memReqTimes(seqNum)} Cycles")
      // println(seqNum)
      issuedLoads -= seqNum 
      completed += seqNum
      nodeStatus(seqNum) = Completed
    }
  }

  def incrementLoadTime(seqNum: Long): Unit = {
    if (memReqTimes.contains(seqNum)){
      memReqTimes(seqNum)= MemReqTime(seqNum, "Load", memReqTimes(seqNum).reqTime + 1)
      if (memReqTimes(seqNum).reqTime >=  WATCH_DOG_TIMEOUT) {
        println(s"Node timed out: ${memReqTimes(seqNum)}")
        debug()
        throw new WatchDogTimeoutError()
      }
    }else{
      memReqTimes(seqNum) = MemReqTime(seqNum, "Load", 1L)
    }
  }

  def isDone: Boolean = (completed.size == nodes.size) && eof

  def log(name: String, seqNum: Long): Unit = {
    MemReqLogger.log(name, memReqTimes(seqNum))
    memReqTimes.remove(seqNum)
  }

  def debug(): Unit ={
    for ((seq, node) <- nodes) {
        if (nodeStatus(seq) != Completed) {
          println(node)
      }
    }
    for ((seq, node) <- nodes) {
      if (nodeStatus(seq) != Completed){
        println(s"NODE: ${seq}, STATUS: ${nodeStatus(seq)}")
      }
    }
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