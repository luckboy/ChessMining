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

case class DrawBoardMiner[-T](
  drawFunction: NamedFunction2[(Game, T), Int, Boolean],
  firstMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None) extends BooleanBoardMiner[(Game, T), DrawBoardMiner[T]]
{
  override def firstAdjective = drawFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("draw")

  override def secondNounOption = None

  override def booleanSquareFunction(x: (Game, T), squ: Int) = x._1.hasDraw && drawFunction(x, squ)

  def +\+[U <: T](function: NamedFunction2[(Game, U), Int, Boolean]) =
    copy(firstMinerOption = Some(DrawBoardMiner(function)))
}
