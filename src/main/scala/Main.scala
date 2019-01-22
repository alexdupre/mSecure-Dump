import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.security.MessageDigest
import java.util.zip.InflaterOutputStream

import com.github.tototoshi.csv.CSVWriter
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex
import org.apache.commons.io.IOUtils

import scala.util.control.NonFatal
import scala.xml.{Node, XML}

object Main extends App {

  case class Dict(entries: Map[String, Any]) {
    def get[T](key: String) = entries(key).asInstanceOf[T]
  }

  case class Type(id: Int, name: String, fields: Seq[String])
  case class Entry(typeId: Int, description: String, fields: Seq[String], note: String)

  def convertEndianess(data: Array[Byte], words: Int) = {
    val src = ByteBuffer.wrap(data)
    src.order(ByteOrder.LITTLE_ENDIAN)
    val dst = ByteBuffer.allocate(words * 4)
    for (_ <- 1 to words) dst.putInt(src.getInt)
    dst.array()
  }

  def getCipher(passphrase: String) = {
    val sha256 = MessageDigest.getInstance("SHA-256")
    val key    = sha256.digest(passphrase.getBytes("UTF-8"))
    val cipher = Cipher.getInstance("Blowfish/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "Blowfish"))
    cipher
  }

  def decrypt(cipher: Cipher, ciphertext: String) = {
    if (ciphertext.isEmpty) ""
    else {
      val bin   = Hex.decodeHex(ciphertext)
      val words = bin.length / 8 * 2
      val len   = bin.length - 7
      val enc   = convertEndianess(bin, words)
      val pad   = cipher.doFinal(enc)
      val dec   = convertEndianess(pad, words)
      val data  = new String(dec, 0, len, "UTF-8")
      data
    }
  }

  def getDBXML(file: File) = {
    val buf = new Array[Byte](file.length().toInt)
    val in  = new FileInputStream(file)
    IOUtils.readFully(in, buf)
    in.close()
    val bb = ByteBuffer.wrap(buf)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.getLong // checksum
    val len  = bb.getLong.toInt
    val baos = new ByteArrayOutputStream()
    val inf  = new InflaterOutputStream(baos)
    inf.write(buf, 24, len)
    inf.close()
    new String(baos.toByteArray, "UTF-8")
  }

  def parseXML(xml: String) = parseNode(XML.loadString(xml)).asInstanceOf[Dict]

  def parseNode(e: Node): Any = e.label match {
    case "plist" => parseNode(e.child.head)
    case "dict" =>
      Dict(
        e.child
          .grouped(2)
          .map {
            case Seq(key, value) => parseNode(key).toString -> parseNode(value)
          }
          .toMap)
    case "array"   => e.child.map(parseNode)
    case "key"     => e.text
    case "integer" => e.text.toInt
    case "string"  => e.text
    case "true"    => true
    case "false"   => false
  }

  def parseTypes(data: Dict) = {
    val types = data.get[Seq[Dict]]("typesList")
    types.map { t =>
      val id     = t.get[Int]("ID")
      val name   = t.get[String]("name")
      val fields = t.get[Seq[Dict]]("fields").map(_.get[String]("name"))
      Type(id, name, fields)
    }
  }

  def parseRecords(data: Dict) = {
    val records = data.get[Seq[Dict]]("recordsList")
    records.map { record =>
      val typeId      = record.get[Int]("typeID")
      val description = decrypt(cipher, record.get[String]("description")).trim
      val fields      = record.get[Seq[String]]("fields").map(field => decrypt(cipher, field).trim)
      val note        = record.get[String]("note").trim
      Entry(typeId, description, fields, note)
    }
  }

  if (args.length != 3) sys.error("Missing arguments: <msim filepath> <passphrase> <output dir>")

  val msimfile   = new File(args(0))
  val passphrase = args(1)
  val outdir     = new File(args(2))

  val cipher = getCipher(passphrase)
  val data = try {
    val xml = getDBXML(msimfile)
    //println(xml)
    parseXML(xml)
  } catch {
    case NonFatal(e) => throw new RuntimeException("Invalid mSecure Backup file", e)
  }
  require(data.get[Int]("version") == 3, "Unsupported mSecure Backup version")
  val types   = parseTypes(data)
  val records = parseRecords(data)

  if (!outdir.exists()) outdir.mkdirs()

  records.groupBy(_.typeId).foreach {
    case (typeId, entries) =>
      types.find(_.id == typeId).foreach { t =>
        val outfile = new File(outdir, t.name.replaceAll("[^a-zA-Z0-9]", "").toLowerCase + ".csv")
        val csv     = CSVWriter.open(outfile)
        csv.writeRow("Description" +: t.fields :+ "Note")
        entries.sortBy(_.description.toLowerCase).foreach(e => csv.writeRow(e.description +: e.fields :+ e.note))
        csv.close()
      }
  }

}
