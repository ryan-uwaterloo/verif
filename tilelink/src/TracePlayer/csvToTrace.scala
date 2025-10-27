package verif.etrace

import java.io._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters._
import scala.io.Source
import com.verif.TraceProtos._
import com.google.protobuf.CodedOutputStream

object CsvToProtoGz {

  def parseIntOpt(s: String): Option[Long] =
    Option(s).map(_.trim).filter(_.nonEmpty).map(str => java.lang.Long.decode(str))

  def parseIntList(s: String): Seq[Long] =
    Option(s).map(_.trim).filter(_.nonEmpty)
      .map(_.split("\\|").toSeq.map(x => java.lang.Long.decode(x).longValue()))
      .getOrElse(Seq.empty)

  def convertCsv(csvPath: String, outputPath: String, msgType: String): Unit = {
    val lines = Source.fromFile(csvPath).getLines().toSeq
    val csv_header = lines.head.split(",").map(_.trim)
    val data = lines.tail.map(_.split(",", -1).map(_.trim))

    val out = new GZIPOutputStream(new FileOutputStream(outputPath))
    // println("Csv Converter here!")

    val magic = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(0xdeadbeef).array()
    out.write(magic)

    msgType match {
      case "dcache" =>
        val header = InstDepRecordHeader.newBuilder()
          .setObjId("core_dcache")        // You choose this string
          .setVer(2)                       // Optional version
          .setTickFreq(1000000000000L)              // Your test clock frequency
          .setWindowSize(576)              // Custom field for dcache header
          .build()

        // Write the serialized header
        val headerBytes = header.toByteArray
        val codedOut = CodedOutputStream.newInstance(out)
        codedOut.writeRawVarint32(headerBytes.length)
        codedOut.flush()
        out.write(headerBytes)

        data.foreach { row =>
          val rowMap = csv_header.zip(row).toMap

          val builder = InstDepRecord.newBuilder()
            .setSeqNum(rowMap("seq_num").toLong)
            .setType(rowMap("type") match {
              case "LOAD"  => InstDepRecord.RecordType.LOAD
              case "STORE" => InstDepRecord.RecordType.STORE
              case "COMP"  => InstDepRecord.RecordType.COMP
              case _       => InstDepRecord.RecordType.INVALID
            })
            .setCompDelay(rowMap("comp_delay").toLong)

          // Optional fields – only set if defined
          parseIntOpt(rowMap.getOrElse("p_addr", "")).foreach(v => builder.setPAddr(v.toLong))
          parseIntOpt(rowMap.getOrElse("size", "")).foreach(v   => builder.setSize(v.toInt))
          parseIntOpt(rowMap.getOrElse("flags", "")).foreach(v  => builder.setFlags(v.toInt))
          parseIntOpt(rowMap.getOrElse("weight", "")).foreach(v => builder.setWeight(v.toInt))
          parseIntOpt(rowMap.getOrElse("pc", "")).foreach(v     => builder.setPc(v.toLong))
          parseIntOpt(rowMap.getOrElse("v_addr", "")).foreach(v => builder.setVAddr(v.toLong))
          parseIntOpt(rowMap.getOrElse("asid", "")).foreach(v   => builder.setAsid(v.toInt))

          // Repeated fields – use addAll
          builder.addAllRobDep(parseIntList(rowMap.getOrElse("rob_dep", "")).map(Long.box).asJava)
          builder.addAllRegDep(parseIntList(rowMap.getOrElse("reg_dep", "")).map(Long.box).asJava)

          val msg = builder.build()
          val bytes = msg.toByteArray
          codedOut.writeRawVarint32(bytes.length)
          codedOut.flush()
          out.write(bytes)
        }

      case "icache" =>
        val header = PacketHeader.newBuilder()
          .setObjId("core_icache")
          .setVer(2)
          .setTickFreq(1000000000000L)
          .build()

        val headerBytes = header.toByteArray
        val codedOut = CodedOutputStream.newInstance(out)
        codedOut.writeRawVarint32(headerBytes.length)
        codedOut.flush()
        out.write(headerBytes)

        data.foreach { row =>
          val rowMap = csv_header.zip(row).toMap

          val builder = Packet.newBuilder()
            .setTick(rowMap("tick").toLong)
            .setCmd(rowMap("cmd").toInt)
            .setAddr(java.lang.Long.decode(rowMap("addr")))
            .setSize(rowMap("size").toInt)

          parseIntOpt(rowMap.getOrElse("flags", "")).foreach(v  => builder.setFlags(v.toInt))
          parseIntOpt(rowMap.getOrElse("pkt_id", "")).foreach(v => builder.setPktId(v.toLong))
          parseIntOpt(rowMap.getOrElse("pc", "")).foreach(v     => builder.setPc(v.toLong))

          val msg = builder.build()
          // println(s"parsed message: ${msg}")
          val bytes = msg.toByteArray
          codedOut.writeRawVarint32(bytes.length)
          codedOut.flush()
          out.write(bytes)
        }

      case _ => throw new IllegalArgumentException(s"Unknown msgType: $msgType")
    }


    out.close()
    println(s"Wrote ${data.size} records to $outputPath")
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 3) {
      println("Usage: CsvToProtoGz <input.csv> <output.proto.gz> <inst|pkt>")
      sys.exit(1)
    }
    val (csvPath, outputPath, msgType) = (args(0), args(1), args(2))
    convertCsv(csvPath, outputPath, msgType)
  }
}
