// package verif.etrace

// import java.io._
// import java.util.zip.GZIPOutputStream
// import scala.io.Source
// import com.verif.TraceProtos._

// object CsvToProtoGz {

//   def parseIntOpt(s: String): Option[Long] =
//     Option(s).map(_.trim).filter(_.nonEmpty).map(str => java.lang.Long.decode(str))

//   def parseIntList(s: String): Seq[Long] =
//     Option(s).map(_.trim).filter(_.nonEmpty)
//       .map(_.split("\\|").toSeq.map(x => java.lang.Long.decode(x)))
//       .getOrElse(Seq.empty)

//   def convertCsv(csvPath: String, outputPath: String, msgType: String): Unit = {
//     val lines = Source.fromFile(csvPath).getLines().toSeq
//     val header = lines.head.split(",").map(_.trim)
//     val data = lines.tail.map(_.split(",", -1).map(_.trim))

//     val out = new GZIPOutputStream(new FileOutputStream(outputPath))

//     msgType match {
//       case "inst" =>
//         data.foreach { row =>
//           val rowMap = header.zip(row).toMap
//           val msg = InstDepRecord(
//             seqNum = rowMap("seq_num").toLong,
//             `type` = rowMap("type") match {
//               case "LOAD" => InstDepRecord.RecordType.LOAD
//               case "STORE" => InstDepRecord.RecordType.STORE
//               case "COMP" => InstDepRecord.RecordType.COMP
//               case _ => InstDepRecord.RecordType.INVALID
//             },
//             pAddr = parseIntOpt(rowMap.getOrElse("p_addr", "")).map(_.toLong),
//             size = parseIntOpt(rowMap.getOrElse("size", "")).map(_.toInt),
//             flags = parseIntOpt(rowMap.getOrElse("flags", "")).map(_.toInt),
//             robDep = parseIntList(rowMap.getOrElse("rob_dep", "")),
//             compDelay = rowMap("comp_delay").toLong,
//             regDep = parseIntList(rowMap.getOrElse("reg_dep", "")),
//             weight = parseIntOpt(rowMap.getOrElse("weight", "")).map(_.toInt),
//             pc = parseIntOpt(rowMap.getOrElse("pc", "")).map(_.toLong),
//             vAddr = parseIntOpt(rowMap.getOrElse("v_addr", "")).map(_.toLong),
//             asid = parseIntOpt(rowMap.getOrElse("asid", "")).map(_.toInt)
//           )
//           out.write(msg.toByteArray)
//         }

//       case "pkt" =>
//         data.foreach { row =>
//           val rowMap = header.zip(row).toMap
//           val msg = Packet(
//             tick = rowMap("tick").toLong,
//             cmd = rowMap("cmd").toInt,
//             addr = java.lang.Long.decode(rowMap("addr")),
//             size = rowMap("size").toInt,
//             flags = parseIntOpt(rowMap.getOrElse("flags", "")).map(_.toInt),
//             pktId = parseIntOpt(rowMap.getOrElse("pkt_id", "")).map(_.toLong),
//             pc = parseIntOpt(rowMap.getOrElse("pc", "")).map(_.toLong)
//           )
//           out.write(msg.toByteArray)
//         }

//       case _ => throw new IllegalArgumentException(s"Unknown msgType: $msgType")
//     }

//     out.close()
//     println(s"Wrote ${data.size} records to $outputPath")
//   }

//   def main(args: Array[String]): Unit = {
//     if (args.length != 3) {
//       println("Usage: CsvToProtoGz <input.csv> <output.proto.gz> <inst|pkt>")
//       sys.exit(1)
//     }
//     val (csvPath, outputPath, msgType) = (args(0), args(1), args(2))
//     convertCsv(csvPath, outputPath, msgType)
//   }
// }
