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

/** A win miner that counts the data element for the won games.
  *
  * The example usages are:
  * {{{
  * val miner = WinMiner(greaterMobility)
  * val data = miner(iter)
  *
  * val miner = WinMiner(lessMobility) +\+ white
  * val data = miner(iter)
  * }}}
  *
  * @tparam T the type of second data value.
  * @param winFunction the win function.
  * @param firstMinerOption the optional first miner. 
  * @param secondMinerOption the optional second miner. 
  *
  */
case class WinMiner[-T](
  winFunction: NamedFunction2[(Game, T), Side.Value, Boolean],
  firstMinerOption: Option[BinaryMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryMiner[(Game, T), _]] = None) extends BooleanMiner[(Game, T), WinMiner[T]]
{
  override def firstAdjective = winFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("win")

  override def secondNounOption = None

  override def booleanFunction(x: (Game, T)) =
    (x._1.hasSideWin(Side.White) && winFunction(x, Side.White)) ||
    (x._1.hasSideWin(Side.Black) && winFunction(x, Side.Black))

  /** Creates a new miner with the first win miner with the win function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +\+[U <: T](fun: NamedFunction2[(Game, U), Side.Value, Boolean]) =
    copy(firstMinerOption = Some(WinMiner(fun)))

  /** Creates a new miner with the second win miner with the win function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +/+[U <: T](fun: NamedFunction2[(Game, U), Side.Value, Boolean]) =
    copy(secondMinerOption = Some(WinMiner(fun)))
}
