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
package pl.luckboy.chessmining.eval
import pl.luckboy.chessmining.chess._

/** This object provides method that counts attacks of king zone. */
object KingZone
{
  /** Counts attacks of king zone.
    *
    * @param board the board.
    * @param side the side.
    * @return number of attacks of king zone.
    */
  def kingZone(board: Board, side: Side.Value) =
    (0 until 64).find { board.coloredPiece(_) == sideAndPieceToColoredPiece(side, Piece.King) }.map {
      (kingSqu: Int) =>
        foldZoneSquares(kingSqu, 0) {
          (sum: Int, zoneSqu: Int) => 
            if(board.hasAttack(side, zoneSqu))
              sum + 1
            else
              sum
        }
    }.getOrElse(0)
}
