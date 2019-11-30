import java.io.File

object Danative {

  type FileExtension  = String
  type FileNumber     = Int
  type FileSizeBytes  = Long

  def extension(f: File): FileExtension = {
    val str = f.getName
    str.lastIndexOf('.') match {
      case -1 => "empty"
      case i => str.slice(i+1, str.length).toLowerCase
    }
  }
  
  def filesUnderDir(f: File): Vector[File] = {
    val here = f.listFiles.toVector
    here.filter(_.isFile) ++ here.filter(_.isDirectory).flatMap(filesUnderDir)
  }
  
  def createResults(extensionsToBytes: Map[FileExtension, (FileNumber, FileSizeBytes)]): Seq[String] = {

    import Console._

    val (extensions, numberOfFilesAndBytes) = extensionsToBytes.unzip
    val (numberOfFiles, bytes) = numberOfFilesAndBytes.unzip 
    val overallSize = bytes.sum
    val filesDiscovered = numberOfFiles.sum
    val seqedData = (extensions zip bytes).toSeq.sortWith(_._2 > _._2)

    def colorize(color: String)(x: String): String = color + x + Console.RESET

    def createLine(prefix: String, extension: String, size: String, percent: String): String =
      f"$prefix $extension%30s  -> $size%15s $percent%5s"
  
    def beautifyPercentage(v: Long): String = 
      s"(${(100.0 * v / overallSize)}%)"

    def beautifyAbsolute(v: Long): String = {
      def fmt(n: Double): String = "%.2f" format n
      val c = 1024.0
      if      (v < c)       v + " B"
      else if (v < c*c)     fmt(v/c) + " KB"
      else if (v < c*c*c)   fmt(v/c/c) + " MB"
      else if (v < c*c*c*c) fmt(v/c/c/c) + " GB"
      else                  fmt(v/c/c/c/c) + " TB"
    }

    val colors: Array[String => String] = 
      Array(RED, BLUE, MAGENTA, GREEN, YELLOW, CYAN) map colorize

    val colored = colors.length

    val coloredData = (seqedData zip colors).map{case ((k, v), colorize) =>
      colorize(
        createLine(
          "@",
          k,
          beautifyAbsolute(v),
          beautifyPercentage(v)
        )
      )
    }

    val simpleData = (seqedData drop colored).map{case (k, v) =>
      createLine(
        "@",
        k,
        beautifyAbsolute(v),
        beautifyPercentage(v)
      )
    }

    val overallInfo = Seq(
      s"\n> Files discovered: $filesDiscovered",
      s"> ${UNDERLINED}Total size: ${beautifyAbsolute(overallSize)}${RESET}"
    )

    coloredData ++ simpleData ++ overallInfo
  }

  def main(args: Array[String]): Unit = 
  createResults {
    filesUnderDir(new File(args.headOption.getOrElse(".")))
          .groupBy(extension)
          .mapValues(files => (files.length, files.map(file => file.length).sum))
  }.foreach(println)
}
