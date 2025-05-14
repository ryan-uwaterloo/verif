package verif

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import scala.collection.mutable
import scala.math.ceil

import scala.io.Source
import scala.collection.mutable.ListBuffer
import TLTransaction._
import chisel3.experimental.BundleLiterals._


package object TLUtils {
  // Helper functions for message checking
  def aligned(data : UInt, base : UInt) : Boolean = {
    val dataI = data.litValue
    val baseI = base.litValue - 1
    ((dataI & baseI) == 0) && contiguous(baseI.U)
  }

  def alignedLg(data : UInt, base : UInt) : Boolean = {
    aligned(data, (1 << base.litValue.toInt).U)
  }

  def contiguousMask(mask : UInt, size : UInt, beatBytes: Int) : Boolean = {
    if (size.litValue > log2Ceil(beatBytes)) {
      (1 << beatBytes) - 1 == mask.litValue
    } else {
      // Need to check all possible contiguous masks
      val totalBytes = 1 << size.litValue.toInt
      val possibleMasks = (0 to (beatBytes - totalBytes)).toList.map(i => ((1 << totalBytes) - 1) << i)
      possibleMasks.map(pmask => pmask == mask.litValue).foldLeft(false)(_ || _)
    }
  }

  def maskWithinSize(mask: UInt, size: UInt, beatBytes: Int): Boolean = {
    if (size.litValue > log2Ceil(beatBytes)) {
      (1 << beatBytes) > mask.litValue
    } else {
      (1 << (1 << size.litValue.toInt)) > mask.litValue
    }
  }

  def contiguous(data : UInt) : Boolean = {
    val dataI = data.litValue
    ((dataI + 1) & ~dataI) == (dataI + 1)
  }

  def contains(sizes: TransferSizes, x: UInt) : Boolean = {
    (x.litValue >= sizes.min && x.litValue <= sizes.max && isPow2(x.litValue))
  }

  def containsLg(sizes: TransferSizes, lg: UInt) : Boolean = {
    contains(sizes, (1 << lg.litValue.toInt).U)
  }

  // Helper method to get size of TLChannel
  def getTLBundleDataSizeBytes (bnd : TLChannel): Int = {
    bnd match {
      case bndc: TLBundleA =>
        1 << bndc.size.litValue.toInt
      case bndc: TLBundleB =>
        1 << bndc.size.litValue.toInt
      case bndc: TLBundleC =>
        1 << bndc.size.litValue.toInt
      case bndc: TLBundleD =>
        1 << bndc.size.litValue.toInt
      case _: TLBundleE =>
        1
    }
  }

  // Helper method to see if transaction is non-Burst
  def isNonBurst (bnd : TLChannel): Boolean = {
    bnd match {
      case bndc: TLBundleA =>
        // Get, AcquireBlock, AcquirePerm
        (bndc.opcode.litValue == 4 || bndc.opcode.litValue == 5 || bndc.opcode.litValue == 6 || bndc.opcode.litValue == 7)
      case bndc: TLBundleB =>
        // Get, ProbeBlock, ProbePerm
        (bndc.opcode.litValue == 4 || bndc.opcode.litValue == 5 || bndc.opcode.litValue == 6 || bndc.opcode.litValue == 7)
      case bndc: TLBundleC =>
        // AccessAck, ProbeAck, Release
        (bndc.opcode.litValue == 0 || bndc.opcode.litValue == 4 || bndc.opcode.litValue == 6)
      case bndc: TLBundleD =>
        // AccessAck, Grant, ReleaseAck
        (bndc.opcode.litValue == 0 || bndc.opcode.litValue == 4 || bndc.opcode.litValue == 6)
      case _: TLBundleE =>
        // Always single message
        true
    }
  }

  // Helper function to map size -> # of beats
  def sizeToBeats (size : UInt, beatBytes : Int = 8): Int = {
    val sizeInt = 1 << size.litValue.toInt
    ceil(sizeInt / beatBytes.toDouble).toInt
  }

  // Helper method to test if given TLBundles (TLChannels) make up a complete TLTransaction
  def isCompleteTLTxn (txns: Seq[TLChannel], beatBytes: Int = 8) : Boolean = {
    // Edge case
    if (txns.isEmpty) return false

    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    // Currently only checks count
    // TODO Add checks for consistency (size, source, opcode, etc)
    txns.length == expectedTxnCount
  }

  // Helper method to get the next complete TLTransaction (list of TLBundles)
  // Returns (completeTxn, updatedSeq)
  def getNextCompleteTLTxn (txns: Seq[TLChannel]): Option[Seq[TLChannel]] = {
    // Edge case
    if (txns.isEmpty) return None

    // TODO Hardcoded, update when configurability is added
    val beatBytes = 8
    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    if (txns.length < expectedTxnCount) {
      None
    } else {
      Some(txns.dropRight(txns.size - expectedTxnCount))
    }
  }

  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // TODO Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsLB = new mutable.ListBuffer[TLChannel]
    txnsLB ++= txns
    var result = new mutable.ListBuffer[List[TLChannel]]

    while (txnsLB.nonEmpty) {
      val txnCount = if (isNonBurst(txnsLB.head)) 1 else ceil(getTLBundleDataSizeBytes(txnsLB.head) / beatBytes.toDouble).toInt

      // Grouping txnCount TLTransactions (of the same type)
      val newList = new mutable.ListBuffer[TLChannel]
      var index = 0
      newList += txnsLB.remove(index)
      // Getting Class as "type"
      val classType = newList.head.getClass
      while (newList.length < txnCount) {
        if (txnsLB(index).getClass.equals(classType)) {
          newList += txnsLB.remove(index)
        } else {
          index += 1
        }
      }

      result += newList.toList
    }

    result.toList
  }

  def toByteMask(mask : UInt) : BigInt = {
    var maskInt = mask.litValue
    var result: BigInt = 0
    var i = 0
    do {
      if ((maskInt & 1) == 1) {
        result = result | 0xff << (i * 8)
      }
      maskInt = maskInt >> 1
      i = i + 1
    } while (maskInt > 0 )

    result
  }

  // Repeat Permissions for given size
  def permRepeater(size: UInt, perm: UInt, beatBytes : Int = 8) : List[UInt] = {
    val sizeInt = 1 << size.litValue.toInt
    var tempResult: BigInt = 0
    var resultList = mutable.ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = (tempResult << 8) | perm.litValue
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += tempResult.U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += tempResult.U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def readData(state: mutable.HashMap[Int,Int], size: UInt, address: UInt, mask: UInt, beatBytes: Int = 8): List[UInt] = {
    val sizeInt = 1 << size.litValue.toInt
    val addressInt = address.litValue.toInt
    val byteMask = toByteMask(mask)
    var tempResult: BigInt = 0
    var resultList = mutable.ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = tempResult | (state.getOrElse(addressInt + i, 0) << (i * 8))
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += (tempResult & byteMask).U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += (tempResult & byteMask).U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def writeData(state: mutable.HashMap[Int,Int], size: UInt, address: UInt, datas: Seq[UInt], masks: Seq[UInt], beatBytes: Int = 8): Unit = {
    val sizeInt = 1 << size.litValue.toInt
    val addressInt = address.litValue.toInt
    var allData: BigInt = 0
    var allMask: BigInt = 0

    assert(datas.length == masks.length, "Data and Mask lists are not of equal lengths.")

    // Condensing all data and masks
    for (i <- 0 until datas.length) {
      allData = allData | (datas(i).litValue << (beatBytes * 8 * i))
      allMask = allMask | (toByteMask(masks(i)) << (beatBytes * 8 * i))
    }

    // Writing all data with masks
    val byteMask = (1 << 8) - 1
    var preMaskData = 0
    var dataMask = 0
    for (i <- 0 until sizeInt) {
      // Getting a byte worth of data
      preMaskData = (allData & byteMask).toInt
      dataMask = (allMask & byteMask).toInt

      // Updating HashMap
      state(addressInt + i) = (state.getOrElse(addressInt + i, 0) & ~dataMask) | (preMaskData & dataMask)

      // Shifting to next byte
      allData  = allData >> 8
      allMask = allMask >> 8
    }
  }

  // Wrapper class that stores mapping of Address --> Permissions (Note: is block aligned)
  // Permissions: 0 - None, 1 - Read (Branch), 2 - Read/Write (Tip), -1 - Waiting for Grant/Ack
  class RWPermState(blockSize: Int = 3) {
    // HashMap implementation
    private val intState = mutable.HashMap[Int,Int]()
    private val mask = ~((1 << blockSize) - 1)

    def getPerm(address: Int): Int = { intState.getOrElse(address & mask, 0) }

    def setPerm(address: Int, permission: Int): Unit = {
      intState(address & mask) = permission
    }

    def getAllAddr: List[Int] = {
      intState.keys.toList
    }

    // For debugging
    def getState: mutable.HashMap[Int,Int] = {
      intState
    }
  }

  def CSVReadAllWithHeaders(fileStream: java.io.InputStream): Seq[Map[String, String]] = {
    val source = Source.fromInputStream(fileStream)
    try {
      val lines = source.getLines().toSeq
      if (lines.isEmpty) return Seq.empty // Return empty if the file is empty

      val headers = lines.head.split(",").map(_.trim)
      lines.tail.map { line =>
        val values = line.split(",").map(_.trim)
        headers.zip(values).toMap
      }
    } finally {
      source.close()
    }
  }

  def getOpcodeMap(channel: String): Map[String, Int] ={
    val mapping = channel match{
      case "A" => TLMessages.a.zipWithIndex.map { case ((key, _), idx) => key -> idx }.toMap
      case "B" => TLMessages.b.zipWithIndex.map { case ((key, _), idx) => key -> idx }.toMap
      case "C" => TLMessages.c.zipWithIndex.map { case ((key, _), idx) => key -> idx }.toMap
      case "D" => TLMessages.d.zipWithIndex.map { case ((key, _), idx) => key -> idx }.toMap
      case _ => Map(("GrantAck" -> 0))//includes E, we won't think too hard about x atm.
    }
    mapping
  }

  def createTransaction(channel: String, opcode: Int, param: Int, source: Int, address: BigInt, data: BigInt, bundleParams : TLBundleParameters): TLChannel = {

    implicit val p: TLBundleParameters = bundleParams
    if(channel == "A"){
      // if(opcode == 0){
      //   return(TLBundleA(opcode))
      // } else if (opcode == 1){
      //   return(Put(source = source, addr = address, data = data, mask = 255))
      // } else if (opcode == 6) {
      //   return(AcquireBlock(param = TLPermission.Grow.NtoT, addr = address, source = source, size = 5))
      // } else {
      //   return(AcquirePerm(param = TLPermission.Grow.NtoT, addr = address, source = source, size = 5))
      // }
      new TLBundleA(bundleParams).Lit(
      _.opcode -> opcode.U,
      _.param -> param.U,
      _.size -> 3.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> 255.U,
      _.corrupt -> 0.B,
      _.data -> data.U
      )
    } else if (channel == "C"){
      //return(ProbeAckData(param = TLPermission.PruneOrReport.TtoN, data = data, addr = address, source = source, size = 5))
      new TLBundleC(bundleParams).Lit(
        _.opcode -> opcode.U,
        _.param -> param.U,
        _.size -> 3.U,
        _.source -> source.U,
        _.address -> address.U,
        //_.mask -> 255.U,
        _.corrupt -> 0.B,
        _.data -> data.U
      )
    } else {
      return(GrantAck(0))
    }
    
    //(TLBundleA(opcode=UInt<3>(0), param=UInt<3>(0), size=UInt<3>(3), source=UInt<1>(0), address=UInt<12>(4), user=BundleMap(), echo=BundleMap(), mask=UInt<8>(255), data=UInt<64>(57005), corrupt=Bool(false)))
//TLBundleA(opcode=UInt<3>(0), param=UInt<3>(0), size=UInt<3>(3), source=UInt<1>(0), address=UInt<12>(4), user=BundleMap(), echo=BundleMap(), mask=UInt<8>(255), data=UInt<64>(57005), corrupt=Bool(false))
    // else if (channel == "C"){

    // } else if (channel == "D"){

    // } else if (channel == "E"){

    // }
  }


  def CSVtoTL(fileStream: java.io.InputStream, params: TLBundleParameters):  Seq[TLChannel]= {
    //val reader = CSVReader.open(new File(filePath))
    //print(filePath)
    val rows = CSVReadAllWithHeaders(fileStream) 
    var messages = Seq[TLChannel]()
    rows.foreach{ row =>
      //println(row)
      val channel = row("channel")
      val opcodeMap = getOpcodeMap(channel)
      val opcode = opcodeMap.getOrElse(row("opcode"), 0) // Default to 0 if not found
      val param = row.get("param").map(_.toInt).getOrElse(0)
      val source = row.get("source").map(_.toInt).getOrElse(0)
      val address = row.get("address").map { s: String => BigInt(s.stripPrefix("0x"), 16) }.getOrElse(BigInt(0))
      val data = row.get("data").map{ s: String => BigInt(s.stripPrefix("0x"), 16) }.getOrElse(BigInt(0))

      val txn = createTransaction(channel, opcode, param, source, address, data, params)
      println(txn)
      messages = messages :+ txn
    }
    //reader.close()
    messages
  }
}
