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

case class LossBoardMiner[-T](
  lossFunction: NamedFunction3[(Game, T), Side.Value, Int, Boolean],
  firstMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None) extends BooleanBoardMiner[(Game, T), LossBoardMiner[T]]
{
  override def firstAdjective = lossFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("loss")

  override def secondNounOption = None

  override def booleanSquareFunction(x: (Game, T), squ: Int) =
    (x._1.hasSideLoss(Side.White) && lossFunction(x, Side.White, squ)) ||
    (x._1.hasSideLoss(Side.Black) && lossFunction(x, Side.Black, squ))

  def +\+[U <: T](fun: NamedFunction3[(Game, U), Side.Value, Int, Boolean]) =
    copy(firstMinerOption = Some(LossBoardMiner(fun)))

  def +/+[U <: T](fun: NamedFunction3[(Game, U), Side.Value, Int, Boolean]) =
    copy(secondMinerOption = Some(LossBoardMiner(fun)))
}
