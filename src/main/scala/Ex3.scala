import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

object Ex3 {

  def main(args: Array[String]): Unit = {
    val books = List(
      "harry_potter_2.txt",
      "harry_potter_3.txt",
    )
    val k = 3
    val kShingles = ListBuffer[Set[String]]()

    //    books.foreach(book => {
    //      kShingles += generateKShingles(loadBook(book), k)
    //    })

    //    val shingleSets = kShingles.toList

    //    shingleSets.foreach(println)

    println(generateHashingFunctions(10, 10))


    generateCharacteristicMatrix(List(Set("1", "2"), Set("4", "5"), Set("5", "1")))

    println(generateSignatures(List(Set("1", "2"), Set("4", "5"), Set("5", "1")), generateHashingFunctions(4, 2)))
  }

  def generateSignatures(shinglesSets: List[Set[String]], hashingFunctions: List[List[Int]]) = {
    val characteristicMatrix = generateCharacteristicMatrix(shinglesSets)
    println(characteristicMatrix.map(_.mkString(", ")).mkString("\n"))
    //    val signatures = Array.ofDim[Int](hashingFunctions.size, shinglesSets.size)
    //    val shinglesList = shinglesSets.map(_.toList)
    val signaturesForHashes = ListBuffer[List[Int]]()

//    hashingFunctions.foreach(func => {
//      println(f"hash func: $func")
//      val signatures = ListBuffer[Int]()
//      characteristicMatrix.foreach(column => {
//        println(f"column: ${column.mkString(", ")}")
//        signatures += func.zip(column).filter(_._2 == 1).map(_._1).min
//        println(f"filtered values ${func.zip(column).filter(_._2 == 1).map(_._1).min}")
//      })
//      signaturesForHashes += signatures.toList
//    })
//    signaturesForHashes

    characteristicMatrix.foreach(column => {
      println(f"column: ${column.mkString(", ")}")
      val signatures = ListBuffer[Int]()
      hashingFunctions.foreach(func => {
        println(f"hash func: $func")
        signatures += func.zip(column).filter(_._2 == 1).map(_._1).min
        println(f"filtered values ${func.zip(column).filter(_._2 == 1).map(_._1).min}")
      })
      signaturesForHashes += signatures.toList
    })
    signaturesForHashes
  }

  def generateCharacteristicMatrix(shinglesSets: List[Set[String]]) = {
    val universalSet = shinglesSets.foldRight(Set[String]()) {
      (initial, next) => initial.union(next)
    }.toList
    val shinglesList = shinglesSets.map(_.toList)
    //    val characteristicMatrix = Array.ofDim[Int](universalSet.size, shinglesSets.size)
    //    for (i <- universalSet.indices) {
    //      for (j <- shinglesList.indices) {
    ////        val shingleColumn = ListBuffer[Int]
    //        if (shinglesList(j).contains(universalSet(i))) {
    //          characteristicMatrix(i)(j) = 1
    ////          shingleColumn +=
    //        } else {
    //          characteristicMatrix(i)(j) = 0
    //        }
    //      }
    //    }
    //    characteristicMatrix

    val characteristicMatrixColumnList = ListBuffer[List[Int]]()

    shinglesList.foreach(shingles => {
      val shingleColumn = ListBuffer[Int]()
      universalSet.foreach(shingle => {
        if (shingles.contains(shingle)) {
          shingleColumn += 1
        } else {
          shingleColumn += 0
        }
      })
      characteristicMatrixColumnList += shingleColumn.toList
    })

    characteristicMatrixColumnList
  }

  def generateHashingFunctions(g: Int, n: Int): List[List[Int]] = {
    val hashingFunctions = ListBuffer[List[Int]]()
    val integerList = List.range(0, g)
    for (_ <- 0 until n) {
      hashingFunctions += Random.shuffle(integerList)
    }
    hashingFunctions.toList
  }

  def jaccardSim(bookA: Set[String], bookB: Set[String]): Float = {
    bookA.intersect(bookB).size.asInstanceOf[Float] / bookA.union(bookB).size // could be double
  }

  def generateKShingles(book: List[String], k: Int): Set[String] = {
    val bookShinglesSet = collection.mutable.Set[String]()
    for (i <- 0 until book.length - k + 1) {
      val shingle = book.slice(i, i + k).mkString(" ")
      bookShinglesSet.add(shingle)
    }
    bookShinglesSet.toSet
  }

  def loadBook(title: String): List[String] = {
    val PATH = "/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_2/src/main/scala/"
    val wordsList = ListBuffer[String]()
    val source = Source.fromFile(PATH + title)
    val lines = source.getLines.toList
    for (line <- lines) {
      val split = line.split(" ")
      for (word <- split) {
        val cleaned = word.replaceAll("\\.(?!.*\\.)", "")
        if (cleaned.nonEmpty) wordsList += cleaned.toLowerCase
      }
    }
    removeStopWords(wordsList)
  }

  def removeStopWords(wordsList: ListBuffer[String]): List[String] = {
    val source = Source.fromFile("/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_2/src/main/scala/stop_words.txt")
    val stopWordsList = try source.getLines.toList finally source.close
    wordsList.filter(!stopWordsList.contains(_)).toList
  }


}
