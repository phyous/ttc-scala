package com.phyous

import org.scalatest.FunSuite

class BoardTest extends FunSuite {

  test("Empty board should not have victory condition") {
    val board = new Board(3)
    assert(board.checkVictory() == Token.U)
  }

  test("Board with 3 horizontal X's should be a win for X team") {
    val board = new Board(3)
    board.set(Token.X, 1, 0)
    board.set(Token.X, 1, 1)
    board.set(Token.X, 1, 2)

    assert(board.checkVictory() == Token.X)
  }

  test("Board with 3 diagonal O's should be a win for O team") {
    val board = new Board(3)
    board.set(Token.O, 0, 0)
    board.set(Token.O, 1, 1)
    board.set(Token.O, 2, 2)

    assert(board.checkVictory() == Token.O)
  }
}
