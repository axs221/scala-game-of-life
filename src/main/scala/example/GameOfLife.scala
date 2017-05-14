package example

object Hello extends App {
  def createBoard()= {
    Array.ofDim[Boolean](10, 10)
  }

  def printBoard(board: Array[Array[Boolean]]) {
    val boardWithXes = board.map(_.map(if (_) "X" else "."))
    println(boardWithXes.foldLeft("")(_ + "\n" + _.mkString(" ")))
  }

  val board = createBoard()
  printBoard(board)
}
