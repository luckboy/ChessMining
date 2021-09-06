/*
 * Chess Mining - Library to data mining for chess games.
 * Copyright (C) 2021 Łukasz Szpakowski
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received copies of the GNU Lesser General Public
 * License and the GNU General Public License along with this library.
 * If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.value
import java.io._
import pl.luckboy.chessmining.chess._

case class BoardNetwork(edgeCounts: Array[Array[Array[Long]]])
{
  private def coloredPieceToIndex(coloredPiece: ColoredPiece.Value) = 
    coloredPiece match {
      case ColoredPiece.Empty       => 0
      case ColoredPiece.WhitePawn   => 1
      case ColoredPiece.WhiteKnight => 2
      case ColoredPiece.WhiteBishop => 3
      case ColoredPiece.WhiteRook   => 4
      case ColoredPiece.WhiteQueen  => 5
      case ColoredPiece.WhiteKing   => 6
      case ColoredPiece.BlackPawn   => 7
      case ColoredPiece.BlackKnight => 8
      case ColoredPiece.BlackBishop => 9
      case ColoredPiece.BlackRook   => 10
      case ColoredPiece.BlackQueen  => 11
      case ColoredPiece.BlackKing   => 12
    }
  
  def edgeCount(side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int) = {
    val coloredPieceIdx1 = coloredPieceToIndex(coloredPiece1)
    val coloredPieceIdx2 = coloredPieceToIndex(coloredPiece2)
    edgeCounts(side.id)(coloredPieceIdx1 * 64 + squ1)(coloredPieceIdx2 *64 + squ2)
  }

  def addToEdgeCount(side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int, value: Long)
  {
    val coloredPieceIdx1 = coloredPieceToIndex(coloredPiece1)
    val coloredPieceIdx2 = coloredPieceToIndex(coloredPiece2)
    edgeCounts(side.id)(coloredPieceIdx1 * 64 + squ1)(coloredPieceIdx2 *64 + squ2) += value
  }

  def save(fileName: String)
  {
    save(new File(fileName))
  }
  
  def save(file: File)
  {
    val bos = new BufferedOutputStream(new FileOutputStream(file))
    val bnw = new BoardNetworkWriter(new OutputStreamWriter(bos, "UTF-8"))
    try {
      bnw.writeBoardNetwork(this)
    } finally {
      bnw.close()
    }
  }
}

object BoardNetwork
{
  def apply(): BoardNetwork = {
    val edgeCounts = Array.fill[Array[Array[Long]]](2)(null)
    for(sideId <- 0 until 2) {
      edgeCounts(sideId) = Array.fill[Array[Long]](13 * 64)(null)
      for(idx <- 0 until (13 * 64)) {
        edgeCounts(sideId)(idx) = Array.fill(13 * 64)(0L)
      }
    }
    BoardNetwork(edgeCounts)
  }
  
  def load(fileName: String): Option[BoardNetwork] = load(new File(fileName))
  
  def load(file: File) = {
    val bis = new BufferedInputStream(new FileInputStream(file))
    val bnr = new BoardNetworkReader(new InputStreamReader(bis, "UTF-8"))
    try {
      bnr.readBoardNetwork()
    } finally {
      bnr.close()
    }
  }
}
