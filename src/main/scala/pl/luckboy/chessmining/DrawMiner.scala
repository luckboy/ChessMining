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

case class DrawMiner[-T](
  drawFunction: NamedFunction1[(Game, T), Boolean],
  firstMinerOption: Option[BinaryMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryMiner[(Game, T), _]] = None) extends BooleanMiner[(Game, T), DrawMiner[T]]
{
  override def firstAdjective = drawFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("draw")

  override def secondNounOption = None

  override def booleanFunction(x: (Game, T)) = x._1.hasDraw && drawFunction(x)

  def +\+[U <: T](function: NamedFunction1[(Game, U), Boolean]) =
    copy(firstMinerOption = Some(DrawMiner(function)))

  def +/+[U <: T](function: NamedFunction1[(Game, U), Boolean]) =
    copy(secondMinerOption = Some(DrawMiner(function)))
}
