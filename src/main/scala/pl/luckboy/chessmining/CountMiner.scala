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

/** A count board miner that counts the data elements for each games.
  *
  * The example usages are:
  * {{{
  * val miner = CountMiner(greaterWhiteMobility)
  * val data = miner(iter)
  *
  * val miner = CountMiner(lessWhiteMobility)
  * val data = miner(iter)
  * }}}
  *
  * @tparam T the type of second data value.
  * @param countFunction the count function.
  * @param firstMinerOption the optional first miner. 
  * @param secondMinerOption the optional second miner. 
  */
case class CountMiner[-T](
  countFunction: NamedFunction1[(Game, T), Boolean],
  firstMinerOption: Option[BinaryMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryMiner[(Game, T), _]] = None) extends BooleanMiner[(Game, T), CountMiner[T]]
{
  override def firstAdjective = countFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("count")

  override def secondNounOption = None

  override def booleanFunction(x: (Game, T)) = countFunction(x)
}
