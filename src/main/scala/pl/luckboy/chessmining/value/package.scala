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
package pl.luckboy.chessmining
import pl.luckboy.chessmining.chess._

package object value
{
  def coloredPieceToIndex(coloredPiece: ColoredPiece.Value) = 
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
    
  def indexToColoredPiece(idx: Int) =
    idx match {
      case 1  => ColoredPiece.WhitePawn
      case 2  => ColoredPiece.WhiteKnight
      case 3  => ColoredPiece.WhiteBishop
      case 4  => ColoredPiece.WhiteRook
      case 5  => ColoredPiece.WhiteQueen
      case 6  => ColoredPiece.WhiteKing
      case 7  => ColoredPiece.BlackPawn
      case 8  => ColoredPiece.BlackKnight
      case 9  => ColoredPiece.BlackBishop
      case 10 => ColoredPiece.BlackRook
      case 11 => ColoredPiece.BlackQueen
      case 12 => ColoredPiece.BlackKing
      case _  => ColoredPiece.Empty
    }
}
