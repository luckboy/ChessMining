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
import pl.luckboy.chessmining._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.ui._

/** An iterator of game reader that reads games.
  *
  * @constructor Creates a new iterator of game reader.
  *
  * @param f the function of creation of game reader.
  * @param fpb the file progress bar.
  * @param sv the show variable.
  */
class GameReaderIterator(f: () => GameReader, fpb: FileProgressBar, sv: ShowVariable) extends NextOptionIterator[Game]
{
  private var function = f
  private var gameReaderOption = None: Option[GameReader]
  private val fileProgressBar = fpb
  private val showVariable = sv
  private var isClosed = false
  private var gameCount = 0L
  private var hasShow = false

  override protected def nextOption() = {
    showVariable.flag match {
      case ShowFlag.Clear =>
        ()
      case ShowFlag.Set =>
        if(!hasShow) {
          fileProgressBar.show()
          hasShow = true
        }
      case ShowFlag.Auto =>
        if(!hasShow && gameCount == 1L) {
          fileProgressBar.show()
          hasShow = true
        }
    }
    if(!isClosed) {
      try {
        val gameReader = gameReaderOption match {
          case None     => function()
          case Some(gr) => gr
        }
        gameReaderOption = Some(gameReader)
        try {
          val gameOptEither = gameReader.readGame()
          gameOptEither match {
            case Right(Some(game)) =>
              gameCount += 1L
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
              try {
                gameReader.close()
                isClosed = true
                None
              } catch {
                case e: IOException =>
                  isClosed = true
                  None
              }
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
      } catch {
        case e: IOException =>
          fileProgressBar.showError("IOException: " + e.getMessage())
          isClosed = true
          None
      }
    } else
      None
  }
}
