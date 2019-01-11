import java.io.File

object Danative {

  def extension(f: File): String = {
    val str = f.getName
    str.lastIndexOf('.') match {
      case -1 => "empty"
      case i => str.slice(i+1, str.length)
    }
  }
  
  def filesUnderDir(f: File): Vector[File] = {
    val here = f.listFiles.toVector
    here.filter(_.isFile) ++ here.filter(_.isDirectory).flatMap(filesUnderDir)
  }
  
  def createResults(extensionsToBytes: Map[String, (Int, Long)]): Seq[String] = {

    import Console._

    val (extensions, numberOfFilesAndBytes) = extensionsToBytes.unzip
    val (numberOfFiles, bytes) = numberOfFilesAndBytes.unzip 
    val maxSize = bytes.max
    val overallSize = bytes.sum
    val filesDiscovered = numberOfFiles.sum
    val maxExtensionLength = extensions.map(_.length).max
    val padding = 20
    val seqedData = (extensions zip bytes).toSeq.sortWith(_._2 > _._2)

    def colorize(color: String)(x: String): String = color + x + Console.RESET

    def createLine(prefix: String, extension: String, size: String, percent: String): String =
      s"$prefix $extension  -> $size $percent"
  
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

    def normalizeStr(initial: String, target: Int = padding): String = {
      val res = target - initial.length
      if (res > 0) " "*res + initial
      else initial
    }

    val colors: Array[String => String] = 
      Array(RED, BLUE, MAGENTA, GREEN, YELLOW, CYAN) map colorize

    val colored = colors.length

    val coloredData = (seqedData zip colors).map{case ((k, v), colorize) =>
      createLine(
        colorize("@"),
        normalizeStr(k), 
        normalizeStr(colorize(beautifyAbsolute(v))), 
        normalizeStr(beautifyPercentage(v))
      )
    }

    val simpleData = (seqedData drop colored).map{case (k, v) =>
      createLine(
        "@",
        normalizeStr(k),
        normalizeStr(beautifyAbsolute(v), target = padding/2 + 1), 
        normalizeStr(beautifyPercentage(v))
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
