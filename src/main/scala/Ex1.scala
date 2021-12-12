import scala.collection.mutable.ListBuffer
import scala.io.Source

object Ex1 {

  def main(args: Array[String]): Unit = {
    println("Task 1")
    val source = Source.fromFile("/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_2/src/main/scala/graph")
    val outputMapReduce = source.getLines
      .toList.map(mapGraphDegrees)
      .reduce(reduceGraphDegrees)

    source.close()

    outputMapReduce.foreach(println)
  }

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
  }

}


