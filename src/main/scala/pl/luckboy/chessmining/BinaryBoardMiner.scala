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

/** A binary board miner.
  *
  * @tparam T the type of data element.
  * @tparam U the type of actual miner.
  */
abstract class BinaryBoardMiner[-T, +U <: BinaryBoardMiner[T, U]] extends BinaryValueMiner[T, Array[Long], U, BinaryBoardMiner[T, _]]
{
  override def startValue = {
    val firstStartValue = firstMinerOption.map { _.startValue }.getOrElse(Vector("" -> Array.fill(64)(0L)))
    val secondStartValue = secondMinerOption.map { _.startValue }.getOrElse(Vector("" -> Array.fill(64)(0L)))
    val tmpNames = names
    (firstStartValue ++ secondStartValue).zipWithIndex.map {
      case (((_: String, xs: Array[Long]), i: Int)) => tmpNames(i) -> xs 
    }
  }

  /** The square function that processes the square of the data element.
    *
    * @param x the value.
    * @param y the data element.
    * @param squ the square.
    * @return the result.
    */
  def squareFunction(x: Vector[(String, Long)], y: T, squ: Int): Vector[(String, Long)]
}
