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

/** An iterator of boards that allows to iterate by the boards from the game. */
trait BoardIterator extends MoveMakingIterator[Board, Board]
{
  override protected def boardToState(board: Board) = board

  override protected def makeMove(state: Board, move: Move) =
    state.makeMove(move) match {
      case Some(nextBoard) => Some((nextBoard, nextBoard))
      case None            => None
    }
}
