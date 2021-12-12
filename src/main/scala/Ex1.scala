import scala.collection.mutable.ListBuffer
import scala.io.Source

object Ex1 {

  def main(args: Array[String]): Unit = {

    // Task 1
    println("Task 1")
    var source = Source.fromFile("/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_2/src/main/scala/graph")
    val outputMapReduce = source.getLines
      .toList.map(mapGraphDegrees)
      .reduce(reduceGraphDegrees)

    source.close()

    outputMapReduce.foreach(println)




    //Task 3
    println("Task 3")
  }

//  private def removeStopWords(wordsList: ListBuffer[String]): ListBuffer[String] ={
//    val source = Source.fromFile(STOP_WORDS_FILENAME)
//    val stopWordsList = try source.getLines.toList finally source.close
//    wordsList.filter(!stopWordsList.contains(_))
//  }


  // Task 1
  def mapGraphDegrees(line: String): collection.mutable.Map[String, GraphDegrees] = {
    val nodes = line.split(" ")
    collection.mutable.Map(
      nodes(0) -> new GraphDegrees(nodes(0), true, 0, 1),
      nodes(1) -> new GraphDegrees(nodes(1), false, 1, 0),
    )
  }

  def reduceGraphDegrees(mappedNodes: collection.mutable.Map[String, GraphDegrees],
                         nextMap: collection.mutable.Map[String, GraphDegrees])
  = {
    nextMap.foreach(graph => {
      mappedNodes.updateWith(graph._1) {
        case Some(graphDegrees: GraphDegrees) => if (graph._2.from) Some(graphDegrees.addOutDeg) else Some(graphDegrees.addInDeg)
        case None => Some(graph._2)
      }
    })
    mappedNodes
    //    val graphDegrees  = collection.mutable.Map[String, GraphDegrees]()
    //    listOfNodes.foreach(tuple => {

    //      graphDegrees.updateWith(tuple._1) {
    //        case Some(degrees) => Some(degrees.addOutDeg)
    //        case None => Some(new GraphDegrees(tuple._1, 0, 1))
    //      }
    //      graphDegrees.updateWith(tuple._2) {
    //        case Some(degrees)  => Some(degrees.addInDeg)
    //        case None => Some(new GraphDegrees(tuple._2, inDeg = 1, outDeg = 0))
    //      }
    //    })
    //    graphDegrees
  }

}


