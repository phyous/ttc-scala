package com.phyous

class Board(val size: Int) {
  private val points: Array[Array[Token.Value]] = Array.ofDim[Token.Value](size, size)

  def set(t: Token.Value, x: Int, y: Int): Unit = {
    points(x)(y) = t
  }

  def checkVictory(): Token.Value = {
    val lines: List[List[Token.Value]] = getHorizontalLines ::: getVerticalLines ::: getDiagonalLines
    lines
      .map { l => checkVictoryMatch(l) }
      .fold(Token.U) { (acc, r) =>
        r match {
          case Token.U => acc
          case _ => r
        }
      }
  }

  /**
    * Print a board that looks something like:
    *      1   2   3
    *    -------------
    * 1  |   |   |   |
    *    -------------
    * 2  |   | X |   |
    *    -------------
    * 3  |   |   |   |
    *    -------------
    */
  val prefixSpaces = 3
  def printBoard(): Unit = {
    val nSpaces = 0.until(prefixSpaces).map(_=>" ").mkString("")
    val lineDelimiter = nSpaces + 0.to((size + 1) * 3 + size-3).map(_ => '-').mkString("")
    val headerIndices = nSpaces + 1.to(size).map(i => s"$i").mkString("  ", "   ","")

    println(headerIndices)
    println(s"$lineDelimiter")
    points.zipWithIndex.foreach{case(x, i) =>
      val printIdx = i + 1
      val row = x.map(t => if (t == null) " " else t.toString).mkString(s"$printIdx  | ", " | ", " |")
      println(s"$row")
      println(s"$lineDelimiter")
    }
  }

  private def boardRange(): Range = {
    Range(0, size)
  }

  private val xVictory: List[Token.Value] = boardRange().map(_ => Token.X).toList
  private val oVictory: List[Token.Value] = boardRange().map(_ => Token.O).toList

  private def checkVictoryMatch(set: List[Token.Value]): Token.Value = {
    set match {
      case `xVictory` => Token.X
      case `oVictory` => Token.O
      case _ => Token.U
    }
  }

  private def getHorizontalLines: List[List[Token.Value]] = {
    boardRange().map { row =>
      boardRange().map(col => points(row)(col)).toList
    }.toList
  }

  private def getVerticalLines: List[List[Token.Value]] = {
    boardRange().map { col =>
      boardRange().map(row => points(row)(col)).toList
    }.toList
  }

  private def getDiagonalLines: List[List[Token.Value]] = {
    List(
      boardRange().map(i => points(i)(i)).toList,
      boardRange().map(i => points(i)(size - 1 - i)).toList)
  }
}