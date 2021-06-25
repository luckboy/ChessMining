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
package pl.luckboy.chessmining.chess

/** This object provides the values of colored pieces with short names. */
object ColoredPieces
{
  /** An empty. */
  val Em = ColoredPiece.Empty
  /** A white pawn. */
  val WP = ColoredPiece.WhitePawn
  /** A white knight. */
  val WN = ColoredPiece.WhiteKnight
  /** A white bishop. */
  val WB = ColoredPiece.WhiteBishop
  /** A white rook. */
  val WR = ColoredPiece.WhiteRook
  /** A white queen. */
  val WQ = ColoredPiece.WhiteQueen
  /** A white king. */
  val WK = ColoredPiece.WhiteKing
  /** A black pawn. */
  val BP = ColoredPiece.BlackPawn
  /** A black knight. */
  val BN = ColoredPiece.BlackKnight
  /** A black bishop. */
  val BB = ColoredPiece.BlackBishop
  /** A black rook. */
  val BR = ColoredPiece.BlackRook
  /** A black queen. */
  val BQ = ColoredPiece.BlackQueen
  /** A black king. */
  val BK = ColoredPiece.BlackKing
}
