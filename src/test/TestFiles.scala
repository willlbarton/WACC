package src.test

import java.io.File

class TestFiles(dir: String) {

  private def listFiles(directory: File): Seq[File] = {
    if (directory.isDirectory) {
      val files = directory.listFiles
      val subdirectories = files.filter(_.isDirectory)
      val filesInSubdirectories = subdirectories.flatMap(listFiles)
      files.filter(_.isFile).toIndexedSeq ++ filesInSubdirectories
    } else {
      Seq.empty
    }
  }

  def getFiles = listFiles(new File(TestFiles.testDir + dir))
}

object TestFiles {
  def apply(dir: String) = new TestFiles(dir).getFiles
  private val testDir = "src/test/test_files/"

  def getLines(file: File) = {
    val source = scala.io.Source.fromFile(file)
    try source.getLines().mkString("\n") finally source.close()
  }
}
