class GraphDegrees(val node: String, val from: Boolean, var inDeg: Int, var outDeg: Int) {
  def addInDeg = {
    inDeg += 1
    this
  }
  def addOutDeg = {
    outDeg += 1
    this
  }

  override def toString: String = f"inDeg($node)=$inDeg, outDeg($node)=$outDeg"
}
