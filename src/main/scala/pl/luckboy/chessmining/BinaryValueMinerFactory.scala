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

/** A factory class of binary value miner.
  *
  * @tparam T the miner type.
  * @tparam U the type of first miner and/or second miner.
  * @tparam V the new miner type.
  */
abstract class BinaryValueMinerFactory[-T, -U, +V]
{
  /** Creates a new binary value miner.
    *
    * @param miner the miner.
    * @param firstMinerOpt the optional first miner.
    * @param secondMinerOpt the optional second miner.
    */
  def apply(miner: T, firstMinerOpt: Option[U], secondMinerOpt: Option[U]): V
}
