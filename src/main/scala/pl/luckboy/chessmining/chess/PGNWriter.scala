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
package pl.luckboy.chessmining.chess
import java.io._

class PGNWriter(w: Writer) extends GameWriter
{
  private val writer = w
  private var column = 0

  private def writeChar(c: Char)
  {
    writer.write(c.toInt)
  }
  
  private def writeString(s: String)
  {
    writer.write(s)
  }
  
  private def ratingsToString(ratings: Vector[Option[Int]]) =
    ratings.map { _.map { _.toString() }.getOrElse("-") }.mkString(":")

  private def timeControlsToString(timeControls: Vector[Option[TimeControl]]) =
    timeControls.map { _.map { _.toString() }.getOrElse("-") }.mkString(":")

  private def writeEmptyOrSpaceOrNewline(s: String)
  {
    if(column == 0 || column + s.length + 1 <= 80) {
      if(column != 0) writeChar(' ')
      column += s.length + (if(column != 0) 1 else 0)
    } else {
      writeChar('\n')
      column = s.length 
    }
  }
 
  private def writeMovesWithVariations(movesWithVariations: Vector[MoveWithVariations], board: Board, resultOpt: Option[Result.Value]): Boolean = {
    var isFirst = true
    var mustBeMoveNumber = false
    var isError = false
    var tmpBoard = board
    for(moveWithVariations <- movesWithVariations if !isError) {
      tmpBoard.makeMove(moveWithVariations.move) match {
        case Some(tmpBoard2) =>
          if(isFirst || mustBeMoveNumber || tmpBoard.side == Side.White) {
            val moveNumberStr = tmpBoard.fullmoveNumber + (if(tmpBoard.side == Side.White) "." else "...") 
            writeEmptyOrSpaceOrNewline(moveNumberStr)
            writeString(moveNumberStr)
          }
          val moveStr = moveWithVariations.move.toSANString(tmpBoard)
          writeEmptyOrSpaceOrNewline(moveStr)
          writeString(moveStr)
          for(movesWithVariations2 <- moveWithVariations.variations if !isError) {
            writeEmptyOrSpaceOrNewline("(")
            writeString("(")
            isError = !writeMovesWithVariations(movesWithVariations2, tmpBoard, None)
          }
          tmpBoard = tmpBoard2
          mustBeMoveNumber = !moveWithVariations.variations.isEmpty
          isFirst = false
        case None =>
          isError = true
      }
    }
    if(!isError) {
      resultOpt match {
        case Some(result) =>
          val resultStr = resultToString(result)
          writeEmptyOrSpaceOrNewline(resultStr)
          writeString(resultStr)
          writeChar('\n')
        case None =>
          writeEmptyOrSpaceOrNewline(")")
          writeString(")")
      }
      true
    } else
      false
  }
    
  override def writeGame(game: Game) = {
    writeString("[Event \"" + game.event + "\"]\n")
    writeString("[Site \"" + game.site + "\"]\n")
    writeString("[Date \"" + game.date + "\"]\n")
    writeString("[Round \"" + game.round + "\"]\n")
    writeString("[White \"" + game.white.mkString(":") + "\"]\n")
    writeString("[Black \"" + game.black.mkString(":") + "\"]\n")
    writeString("[Result \"" + resultToString(game.result) + "\"]\n")
    for(eventDate <- game.eventDateOption) {
      writeString("[EventDate \"" + eventDate + "\"]\n") 
    }
    for(whiteElo <- game.whiteEloOption) {
      writeString("[WhiteElo \"" + ratingsToString(whiteElo) + "\"]\n") 
    }
    for(blackElo <- game.blackEloOption) {
      writeString("[BlackElo \"" + ratingsToString(blackElo) + "\"]\n") 
    }
    for(whiteUSCF <- game.whiteUSCFOption) {
      writeString("[WhiteUSCF \"" + ratingsToString(whiteUSCF) + "\"]\n") 
    }
    for(blackUSCF <- game.blackUSCFOption) {
      writeString("[BlackUSCF \"" + ratingsToString(blackUSCF) + "\"]\n") 
    }
    for(eco <- game.ecoOption) {
      writeString("[ECO \"" + eco + "\"]\n")
    }
    for(timeControl <- game.timeControlOption) {
      writeString("[TimeControl \"" + timeControlsToString(timeControl) + "\"]\n")
    }
    for(board <- game.boardOption) {
      writeString("[SetUp \"1\"]\n")
      writeString("[FEN \"" + board.toString() + "\"]\n")
    }
    val tags = game.tags -- Set(
      "Event", "Site", "Date", "Round", "White", "Black", "Result", "EventDate", "WhiteElo",
      "BlackElo", "WhiteUSCF", "BlackUSCF", "ECO", "TimeControl", "SetUp", "FEN")
    for((name, value) <- tags) {
      writeString("[" + name + " \"" + value + "\"]\n")
    }
    writeChar('\n')
    column = 0
    if(writeMovesWithVariations(game.movesWithVariations, game.boardOption.getOrElse(Board.Initial), Some(game.result))) {
      writeChar('\n')
      true
    } else
      false
  }

  override def close()
  {
    writer.close()
  }
}
