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

/** A loss miner that counts the data element for the lost games.
  *
  * The example usages are:
  * {{{
  * val miner = LossMiner(greaterMobility)
  * val data = miner(iter)
  *
  * val miner = LossMiner(lessMobility) +\+ white
  * val data = miner(iter)
  * }}}
  *
  * @tparam T the type of second data value.
  * @param lossFunction the loss function.
  * @param firstMinerOption the optional first miner. 
  * @param secondMinerOption the optional second miner. 
  */
case class LossMiner[-T](
  lossFunction: NamedFunction2[(Game, T), Side.Value, Boolean],
  firstMinerOption: Option[BinaryMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryMiner[(Game, T), _]] = None) extends BooleanMiner[(Game, T), LossMiner[T]]
{
  override def firstAdjective = lossFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("loss")

  override def secondNounOption = None

  override def booleanFunction(x: (Game, T)) =
    (x._1.hasSideLoss(Side.White) && lossFunction(x, Side.White)) ||
    (x._1.hasSideLoss(Side.Black) && lossFunction(x, Side.Black))

  /** Creates a new miner with the first loss miner with the loss function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +\+[U <: T](fun: NamedFunction2[(Game, U), Side.Value, Boolean]) =
    copy(firstMinerOption = Some(LossMiner(fun)))

  /** Creates a new miner with the second loss miner with the loss function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +/+[U <: T](fun: NamedFunction2[(Game, U), Side.Value, Boolean]) =
    copy(secondMinerOption = Some(LossMiner(fun)))
}
