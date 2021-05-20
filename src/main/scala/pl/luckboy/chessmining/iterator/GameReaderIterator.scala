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
import java.io._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.ui._

class GameReaderIterator(gr: GameReader, fpb: FileProgressBar) extends NextOptionIterator[Game]
{
  private val gameReader = gr
  private val fileProgressBar = fpb
  private var isClosed = false

  override protected def nextOption() = {
    if(!isClosed) {
      try {
        val gameOptEither = gameReader.readGame()
        gameOptEither match {
          case Right(Some(game)) =>
            Some(game)
          case Right(None)       =>
            try {
              gameReader.close()
              isClosed = true
              None
            } catch {
              case e: IOException =>
                isClosed = true
                None
            }
          case Left(error)       => 
            fileProgressBar.showError(error.toString())
            None
        }
      } catch {
        case e: IOException =>
          fileProgressBar.showError("IOException: " + e.getMessage())
          try {
            gameReader.close()
            isClosed = true
            None
          } catch {
            case e2: IOException =>
              isClosed = true
              None
          }
      }
    } else
      None
  }
}
