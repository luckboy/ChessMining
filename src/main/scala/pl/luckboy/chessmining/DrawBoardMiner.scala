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

/** A draw board miner that counts the squares of data elements for the drawn games.
  *
  * The example usages are:
  * {{{
  * val miner = DrawBoardMiner(anyWhitePiece)
  * val data = miner(iter)
  *
  * val miner = DrawBoardMiner(whitePawn)
  * val data = miner(iter)
  * }}}
  *
  * @tparam T the type of second data value.
  * @param drawFunction the draw function.
  * @param firstMinerOption the optional first miner. 
  * @param secondMinerOption the optional second miner. 
  */
case class DrawBoardMiner[-T](
  drawFunction: NamedFunction2[(Game, T), Int, Boolean],
  firstMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None,
  secondMinerOption: Option[BinaryBoardMiner[(Game, T), _]] = None) extends BooleanBoardMiner[(Game, T), DrawBoardMiner[T]]
{
  override def firstAdjective = drawFunction.name

  override def secondAdjective = "other"

  override def firstNounOption = Some("draw")

  override def secondNounOption = None

  override def booleanSquareFunction(x: (Game, T), squ: Int) = x._1.hasDraw && drawFunction(x, squ)

  /** Creates a new miner with the first draw board miner with the draw function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +\+[U <: T](fun: NamedFunction2[(Game, U), Int, Boolean]) =
    copy(firstMinerOption = Some(DrawBoardMiner(fun)))

  /** Creates a new miner with the second draw board miner with the draw function from this miner.
    *
    * @tparam U the type of second data value for a new miner.
    * @param fun the function.
    * @return a new miner.
    */
  def +/+[U <: T](fun: NamedFunction2[(Game, U), Int, Boolean]) =
    copy(secondMinerOption = Some(DrawBoardMiner(fun)))
}
