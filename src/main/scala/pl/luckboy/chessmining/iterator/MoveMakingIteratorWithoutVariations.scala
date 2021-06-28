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
package pl.luckboy.chessmining.iterator
import pl.luckboy.chessmining.chess._

/** An interator of move making without the variations.
  *
  * @tparam T the element type.
  * @tparam U the state type.
  */
trait MoveMakingIteratorWithoutVariations[T, U] extends MoveMakingIterator[T, U]
{
  private var movesWithVariations = Vector[MoveWithVariations]()
  private var index = 0
  private var stateOption = None: Option[U]

  override protected def initialize(game: Game)
  {
    movesWithVariations = game.movesWithVariations
    index = 0
    stateOption = Some(boardToState(game.boardOption.getOrElse(Board.Initial)))
  }

  override protected def nextOption() =
    if(index < movesWithVariations.length) {
      stateOption match {
        case Some(state) =>
          makeMove(state, movesWithVariations(index).move) match {
            case Some((x, newState)) =>
              index += 1
              stateOption = Some(newState)
              Some(x)
            case None =>
              stateOption = None
              None
          }
        case None =>
          None
      }
    } else
      None
}
