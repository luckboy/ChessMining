/*
 * Chess Mining - Library to data mining for chess games.
 * Copyright (C) 2021 Łukasz Szpakowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.chess

object ColoredPiece extends Enumeration
{
  val Empty = Value(0)
  val WhitePawn = Value(8 | 0)
  val WhiteKnight = Value(8 | 1)
  val WhiteBishop = Value(8 | 2)
  val WhiteRook = Value(8 | 3)
  val WhiteQueen = Value(8 | 4)
  val WhiteKing = Value(8 | 5)
  val BlackPawn = Value(16| 0)
  val BlackKnight = Value(16 | 1)
  val BlackBishop = Value(16 | 2)
  val BlackRook = Value(16 | 3)
  val BlackQueen = Value(16 | 4)
  val BlackKing = Value(16 | 5)
}
