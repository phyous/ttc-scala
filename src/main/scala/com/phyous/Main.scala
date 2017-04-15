package com.phyous

object Main {
  def main(args: Array[String]): Unit = {
    playGame()
  }

  def playGame(): Unit = {
    val board = new Board(3)

    var turn = true
    while (true) {
      val token = if (turn) Token.X else Token.O
      board.printBoard()

      println(s"$token's turn:")
      var validTurn = false
      while(!validTurn) {
        print("row>")
        val row = scala.io.StdIn.readInt()
        print("column>")
        val col = scala.io.StdIn.readInt()
        board.set(token, row-1, col-1)
        if (board.checkVictory() != Token.U) {
          println(s"$token wins!")
          board.printBoard()
          System.exit(0)
        }
        validTurn = true
      }

      turn = !turn
    }
  }
}

