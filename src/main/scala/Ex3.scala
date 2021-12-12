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

    //    println(generateHashingFunctions(10, 10))


    generateCharacteristicMatrix(List(Set("1", "2"), Set("4", "5"), Set("5", "1")))

    //    println(generateSignatures(List(Set("1", "2"), Set("4", "5"), Set("5", "1")), generateHashingFunctions(4, 2)))

    generateCharacteristicMatrix(List(
      generateKShingles(loadBook("doc1.txt"), k),
      generateKShingles(loadBook("doc2.txt"), k),
      generateKShingles(loadBook("doc3.txt"), k)))._1.foreach(println)

    val signatures = generateSignatures(List(
      generateKShingles(loadBook("doc1.txt"), k),
      generateKShingles(loadBook("doc2.txt"), k),
      generateKShingles(loadBook("doc3.txt"), k)),
      10)

    signatures.foreach(println)
    println(f"Signature similarity for doc1.txt and doc3.txt: ${jaccardSim(signatures.head, signatures(2))}")

  }

  def generateSignatures(shinglesSets: List[Set[String]], noOfHashingFunctions: Int) = {
    val (characteristicMatrix,  universalSetSize) = generateCharacteristicMatrix(shinglesSets)
    val hashingFunctions = generateHashingFunctions(universalSetSize, noOfHashingFunctions)
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

    (characteristicMatrixColumnList, universalSet.size)
  }

  def generateHashingFunctions(g: Int, n: Int): List[List[Int]] = {
    val hashingFunctions = ListBuffer[List[Int]]()
    val integerList = List.range(0, g)
    for (_ <- 0 until n) {
      hashingFunctions += Random.shuffle(integerList)
    }
    hashingFunctions.toList
  }

  def jaccardSim(bookA: List[Int], bookB: List[Int]): Float = {
    bookA.zip(bookB).map { case (a, b) => a == b }.count(_ == true).asInstanceOf[Float] / bookA.size
  }

  def generateKShingles(book: List[String], k: Int): Set[String] = {
//    println(f"book: $book")
    val bookShinglesSet = collection.mutable.Set[String]()
    for (i <- 0 until book.length - k + 1) {
      val shingle = book.slice(i, i + k).mkString(" ")
      bookShinglesSet.add(shingle)
    }

    bookShinglesSet.toSet
  }

  def loadBook(title: String, cleanStopWords: Boolean = false): List[String] = {
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
    //    removeStopWords(wordsList)
    if (cleanStopWords) {
      return removeStopWords(wordsList)
    }
    wordsList.toList
  }

  def removeStopWords(wordsList: ListBuffer[String]): List[String] = {
    val source = Source.fromFile("/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_2/src/main/scala/stop_words.txt")
    val stopWordsList = try source.getLines.toList finally source.close
    wordsList.filter(!stopWordsList.contains(_)).toList
  }


}
