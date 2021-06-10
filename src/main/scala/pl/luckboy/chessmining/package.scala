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
package pl.luckboy
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.ui._

package object chessmining
{
  implicit val defaultGameReaderFactory = PGNReader
  implicit val defaultGameWriterFactory = PGNWriter
  implicit val defaultFileProgressBarFactory = ConsoleFileProgressBar
  
  def side(side: Side.Value) = {
    val name = if(side == Side.White) "white" else "black"
    NamedFunction2(name, {
      (tuple: (Game, Any), side2: Side.Value) => side == side2
    })
  }

  def side3(side: Side.Value) = {
    val name = if(side == Side.White) "white" else "black"
    NamedFunction3(name, {
      (tuple: (Game, Any), side2: Side.Value, squ: Int) => side == side2
    })
  }

  val white = side(Side.White)
  val black = side(Side.Black)
  val white3 = side3(Side.White)
  val black3 = side3(Side.Black)

  def anyPiece = NamedFunction3("piece", {
    (tuple: (Game, Board), side: Side.Value, squ: Int) =>
      tuple match {
        case (_, board) => board.color(squ) == sideToColor(side)
      }
  })
  
  def piece(piece: Piece.Value) = {
    val name = piece match {
      case Piece.Pawn   => "pawn"
      case Piece.Knight => "knight"
      case Piece.Bishop => "bishop"
      case Piece.Rook   => "rook"
      case Piece.Queen  => "queen"
      case Piece.King   => "king"
    }
    NamedFunction3(name, {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.coloredPiece(squ) == sideAndPieceToColoredPiece(side, piece)
        }
    })
  }
  
  val pawn = piece(Piece.Pawn)
  val knight = piece(Piece.Knight)
  val bishop = piece(Piece.Bishop)
  val rook = piece(Piece.Rook)
  val queen = piece(Piece.Queen)
  val king = piece(Piece.King)
}
