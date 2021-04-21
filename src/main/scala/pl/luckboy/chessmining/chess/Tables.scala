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

private object Tables
{
  val Mailbox = Array(
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  8,  9, 10, 11, 12, 13, 14, 15, -1,
      -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
      -1, 24, 25, 26, 27, 28, 29, 30, 31, -1,
      -1, 32, 33, 34, 35, 36, 37, 38, 39, -1,
      -1, 40, 41, 42, 43, 44, 45, 46, 47, -1,
      -1, 48, 49, 50, 51, 52, 53, 54, 55, -1,
      -1, 56, 57, 58, 59, 60, 61, 62, 63, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      
  val Mailbox64 = Array(
      21, 22, 23, 24, 25, 26, 27, 28,
      31, 32, 33, 34, 35, 36, 37, 38,
      41, 42, 43, 44, 45, 46, 47, 48,
      51, 52, 53, 54, 55, 56, 57, 58,
      61, 62, 63, 64, 65, 66, 67, 68,
      71, 72, 73, 74, 75, 76, 77, 78,
      81, 82, 83, 84, 85, 86, 87, 88,
      91, 92, 93, 94, 95, 96, 97, 98)

  val TabPawnCaptureSteps120 = Array(Array(11, 9), Array(-9, -11))

  val TabKnightSteps120 = Array(21, 19, 12, 8, -8, -12, -19, -21)

  val TabBishopSteps120 = Array(11, 9, -9, -11)

  val TabRookSteps120 = Array(10, 1, -1, -10)

  val TabQueenSteps120 = Array(11, 10, 9, 1, -1, -9, -10, -11)

  val TabKingSteps120 = Array(11, 10, 9, 1, -1, -9, -10, -11)
}
