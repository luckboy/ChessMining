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

/** A draw miner that counts the data elements for the drawn games.
  *
  * The example usages are:
  * {{{
  * val miner = DrawMiner(greaterWhiteMobility)
  * val data = miner(iter)
  *
  * val miner = DrawMiner(lessWhiteMobility)
  * val data = miner(iter)
  * }}}
  *
  * @tparam T the type of second data value.
  * @param drawFunction the draw function.
  * @param firstMinerOption the optional first miner. 
  * @param secondMinerOption the optional second miner. 
  */
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

  /** Creates a new miner with the first draw miner with the draw function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +\+[U <: T](fun: NamedFunction1[(Game, U), Boolean]) =
    copy(firstMinerOption = Some(DrawMiner(fun)))

  /** Creates a new miner with the second draw miner with the draw function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +/+[U <: T](fun: NamedFunction1[(Game, U), Boolean]) =
    copy(secondMinerOption = Some(DrawMiner(fun)))
}
