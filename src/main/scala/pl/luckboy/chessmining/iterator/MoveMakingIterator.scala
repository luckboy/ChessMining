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

/** An interator of move making that makes the moves from the game during iterations.
  *
  * @tparam T the element type.
  * @tparam U the state type.
  */
trait MoveMakingIterator[T, U] extends NextOptionIterator[T]
{
  /** Initializes this iteraror.
    *
    * @param game the game.
    */
  protected def initialize(game: Game): Unit

  /** Converts the board to a state.
    *
    * @param board the board.
    * @return a state.
    */
  protected def boardToState(board: Board): U

  /** Make move the move.
    *
    * @param state the state.
    * @param move the move.
    * @return an optional element and an optional state. 
    */
  protected def makeMove(state: U, move: Move): Option[(T, U)]
}
