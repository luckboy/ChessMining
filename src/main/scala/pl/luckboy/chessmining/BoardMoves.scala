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
import pl.luckboy.chessmining.iterator._

/** This object provides a method that creates an iterator of board moves without the variations.
  *
  * The example usage is:
  * {{{
  * val iter = BoardMoves.fromGame(game)
  * }}}
  */
object BoardMoves
{
  /** Creates an iterator of board moves without the variations from the game.
    *
    * @param game the game.
    * @return an iterator of board moves.
    */
  def fromGame(game: Game) = new BoardMoveIteratorWithoutVariations(game)
}
