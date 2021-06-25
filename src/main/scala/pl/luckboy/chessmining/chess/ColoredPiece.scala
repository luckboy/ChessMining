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

/** A colored piece enumeration. */
object ColoredPiece extends Enumeration
{
  /** An empty. */
  val Empty = Value(0)
  /** A white pawn. */
  val WhitePawn = Value(8 | 0)
  /** A white knight. */
  val WhiteKnight = Value(8 | 1)
  /** A white bishop. */
  val WhiteBishop = Value(8 | 2)
  /** A white rook. */
  val WhiteRook = Value(8 | 3)
  /** A white queen. */
  val WhiteQueen = Value(8 | 4)
  /** A white king. */
  val WhiteKing = Value(8 | 5)
  /** A black pawn. */
  val BlackPawn = Value(16| 0)
  /** A black knight. */
  val BlackKnight = Value(16 | 1)
  /** A black bishop. */
  val BlackBishop = Value(16 | 2)
  /** A black rook. */
  val BlackRook = Value(16 | 3)
  /** A black queen. */
  val BlackQueen = Value(16 | 4)
  /** A black king. */
  val BlackKing = Value(16 | 5)
}
