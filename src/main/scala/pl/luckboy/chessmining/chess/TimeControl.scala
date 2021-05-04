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

sealed abstract class TimeControl

case object UnknownTimeControl extends TimeControl
case class ClassicalTimeControl(moveCount: Int, seconds: Int) extends TimeControl
case class SuddenDeathTimeControl(seconds: Int) extends TimeControl
case class IncrementalTimeControl(seconds: Int, extraSeconds: Int) extends TimeControl
case class HourglassTimeControl(seconds: Int) extends TimeControl
