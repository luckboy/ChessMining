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

/** A boolean miner that counts results of boolean function. 
  *
  * @tparam T the type of data element.
  * @tparam U the type of actual miner.
  */
abstract class BooleanMiner[-T, +U <: BooleanMiner[T, U]] extends BinaryMiner[T, U]
{
  /** A boolean function that returns a boolean value for the data element.
    *
    * @param x the data element.
    * @return a boolean value.
    */
  def booleanFunction(x: T): Boolean

  def function(x: Vector[(String, Long)], y: T) = {
    val firstCount = firstMinerOption.map { _.count }.getOrElse(1)
    val secondCount = secondMinerOption.map { _.count }.getOrElse(1)
    if(booleanFunction(y)) {
      val firstValue = firstMinerOption.map {
        _.function(x.slice(0, firstCount), y)
      }.getOrElse(Vector(x(0)._1 -> (x(0)._2 + 1L)))
      (0 until firstCount).foldLeft(x) {
        (x2: Vector[(String, Long)], i: Int) => x2.updated(i, x2(i)._1 -> firstValue(i)._2)
      }
    } else {
      val secondValue = secondMinerOption.map {
        _.function(x.slice(firstCount, firstCount + secondCount), y)
      }.getOrElse(Vector(x(firstCount)._1 -> (x(firstCount)._2 + 1L)))
      (firstCount until (firstCount + secondCount)).foldLeft(x) {
        (x2: Vector[(String, Long)], i: Int) => x2.updated(i, x2(i)._1 -> secondValue(i - firstCount)._2)
      }
    }
  }
}
