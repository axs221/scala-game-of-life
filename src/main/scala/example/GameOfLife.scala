package example

import scala.collection.immutable.IndexedSeq

object Hello extends App {
  def createBoard()= {
    val r = scala.util.Random
    1 to 20 map { n => 1 to 20 map { c => r.nextInt(2) }}
  }

  def printBoard(board: IndexedSeq[IndexedSeq[Int]]) {
    val boardWithXes = board.map(_.map((cell) => if (cell == 1) "X" else "."))
    println(boardWithXes.foldLeft("")(_ + "\n" + _.mkString(" ")))
  }

  def getValue(board: IndexedSeq[IndexedSeq[Int]], x: Int, y: Int) =
    board.lift(x).getOrElse[IndexedSeq[Int]](IndexedSeq[Int]()).lift(y).getOrElse(0)

  def getNeighborCoordinates (xa: Int, ya: Int) =
    for (x <- -1 to 1; y <- -1 to 1 if x != 0 || y != 0)
      yield (xa + x, ya + y)

  def countNeighbors(board: IndexedSeq[IndexedSeq[Int]], x: Int, y: Int)= {
    getNeighborCoordinates(x, y)
      .foldLeft(0)((prev, values) => prev + getValue(board, values._1, values._2))
  }

  def determineIfCellIsAlive(board: IndexedSeq[IndexedSeq[Int]], x: Int, y: Int) = {
    val previousValue = getValue(board, x, y)
    val isAlive = previousValue == 1
    val neighborsAlive = countNeighbors(board, x, y)

    if (isAlive && neighborsAlive < 2) 0
    else if (isAlive && (neighborsAlive == 2 || neighborsAlive == 3)) 1
    else if (isAlive && (neighborsAlive > 3)) 0
    else if (!isAlive && (neighborsAlive == 3)) 1
    else if (!isAlive && (neighborsAlive == 3)) 1
    else previousValue
  }

  def runIteration(board: IndexedSeq[IndexedSeq[Int]]) = {
    0 until board.length map { x => 0 until board(x).length map { y => determineIfCellIsAlive(board, x, y) } }
  }

  var board = createBoard()
  printBoard(board)

  for (x <- 1 to 100) {
    board = runIteration(board)
    printBoard(board)
    Thread sleep 120
  }
}
