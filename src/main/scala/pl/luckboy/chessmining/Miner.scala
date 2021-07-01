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

/** A miner is a object that processes data. The miner uses the `foldLeft` method for data processing.
  *
  * @tparam T the type of data element.
  * @tparam U the result type.
  */
abstract class Miner[-T, U]
{
  /** Returns the start value.
    *
    * @return the start value.
    */
  def startValue: U
  
  /** A function that processes the data element.
    *
    * @param x the value.
    * @param y the data element.
    */
  def function(x: U, y: T): U
  
  /** Processes the data.
    *
    * @param data the data.
    * @return the result.
    */
  def apply(data: TraversableOnce[T]) = data.foldLeft(startValue)(function)
}
