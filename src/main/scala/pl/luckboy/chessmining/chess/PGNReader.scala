/*
 * Chess Mining - Library to data mining for chess games.
 * Copyright (C) 2021 Łukasz Szpakowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.chess
import java.io._
import scala.collection.mutable.Stack

class PGNReader(r: Reader) extends GameReader
{
  private abstract class Token
  {
    def lineNumber: Int
  }

  private case class SymbolToken(s: String, lineNumber: Int) extends Token
  private case class StringToken(s: String, lineNumber: Int) extends Token
  private case class NAGToken(s: String, lineNumber: Int) extends Token
  private case class OtherToken(c: Char, lineNumber: Int) extends Token
  private case class EOFToken(lineNumber: Int) extends Token

  private case class Tag(name: String, value: String)
  
  private val reader = r
  private val pushedChars = Stack[Char]()
  private val pushedTokens = Stack[Token]()
  private var lineNumber = 1
  
  private def readChar() =
    if(!pushedChars.isEmpty) {
      pushedChars.pop().toInt
    } else {
      val i = reader.read()
      if(i.toChar == '\n') lineNumber += 1
      i
    }

  private def unreadChar(c: Int)
  {
    if(c != -1) {
      if(c.toChar == '\n') lineNumber -= 1
      pushedChars.push(c.toChar)
    }
  }

  private def skipSpacesAndComments()
  {
    var isStop = false
    while(!isStop) {
      val i = readChar()
      if(i == -1) {
        isStop = true
      } else {
        val c = i.toChar
        if(c != ' ' && c != '\t' && c != '\u000B' && c != '\n' && c != '\r' && c != ';' && c != '{') {
          unreadChar(i)
          isStop = true
        } else if(c == '\n') {
          val i2 = readChar()
          if(i2 != -1) {
            val c2 = readChar()
            if(c == '%') {
              var isStop2 = false
              while(!isStop2) {
                val i3 = readChar()
                if(i3 == -1) {
                  isStop2 = true
                } else {
                  val c = i3.toChar
                  if(c == '\n') isStop2 = true
                }
              }
            } else
              unreadChar(i2)
          }
        } else if(c == ';') {
          var isStop2 = false
          while(!isStop2) {
            val i2 = readChar()
            if(i2 == -1) {
              isStop2 = true
            } else {
              val c2 = i2.toChar
              if(c == '\n') isStop2 = true
            }
          }
        } else if(c == '{') {
          var isStop2 = false
          while(!isStop2) {
            val i2 = readChar()
            if(i2 == -1) {
              isStop2 = true
            } else {
              val c2 = i2.toChar
              if(c == '}') isStop2 = true
            }
          }
        }
      }
    }
  }

  private def readToken() = {
    if(!pushedTokens.isEmpty) {
      Right(pushedTokens.pop())
    } else {
      skipSpacesAndComments()
      val i = readChar()
      if(i == -1) {
        Right(EOFToken(lineNumber))
      } else {
        val c = i.toChar
        if((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) {
          val tmpLineNumber = lineNumber
          var sb = new StringBuilder()
          sb += c
          var isStop = false
          while(!isStop) {
            val i2 = readChar()
            if(i2 == -1) {
              isStop = true
            } else {
              val c2 = i2.toChar
              if((c2 >= 'A' && c2 <= 'Z') || (c2 >= 'a' && c2 <= 'z') || (c2 >= '0' && c2 <= '9') ||
                c2 == '_' || c2 == '+' || c2 == '#' || c2 == '=' || c2 == ':' || c2 == '-' ||
                c2 == '/' || c2 == '?' || c2 == '!') {
                sb += c2
              } else {
                unreadChar(i)
                isStop = true
              }
            }
          }
          Right(SymbolToken(sb.toString(), tmpLineNumber))
        } else if(c == '"') {
          val tmpLineNumber = lineNumber
          val sb = new StringBuilder()
          var isStop = false
          var isUnclosedString = false
          while(!isStop) {
            val i2 = readChar()
            if(i2 == -1) {
              isUnclosedString = true
              isStop = true
            } else {
              val c2 = i2.toChar
              if(c == '"')
                isStop = true
              else
               sb += c2
            }
          }
          if(!isUnclosedString)
            Right(StringToken(sb.toString(), tmpLineNumber))
          else
            Left(PGNReaderError(tmpLineNumber, "Unclosed string"))
        } else if(c == '$') {
          val tmpLineNumber = lineNumber
          var sb = new StringBuilder()
          sb += c
          var isStop = false
          while(!isStop) {
            val i2 = readChar()
            if(i2 == -1) {
              isStop = true
            } else {
              val c2 = i2.toChar
              if(c2 >= '0' && c2 <= '9') {
                sb += c2
              } else {
                unreadChar(i)
                isStop = true
              }
            }
          }
          Right(NAGToken(sb.toString(), tmpLineNumber))
        } else if(c == '.' || c == '*' ||
          c == '[' || c == ']' || c == '(' || c == ')' || c == '<' || c >= '>') {
          Right(OtherToken(c, lineNumber))
        } else
          Left(PGNReaderError(lineNumber, "Unrecognized character"))
      }
    }
  }

  private def unreadToken(token: Token)
  {
    pushedTokens.push(token)
  }
  
  private def readTag() =
    readToken() match {
      case Right(OtherToken('[', tmpLineNumber)) =>
        readToken() match {
          case Right(SymbolToken(name, _)) =>
            readToken() match {
              case Right(StringToken(value, _)) =>
                readToken() match {
                  case Right(OtherToken(']', _)) =>
                    Right(Some((Tag(name, value), tmpLineNumber)))
                  case Right(token4) =>
                    Left(PGNReaderError(token4.lineNumber, "Unexpected token"))
                  case Left(error) =>
                    Left(error)
                }
              case Right(token3) =>
                Left(PGNReaderError(token3.lineNumber, "Unexpected token"))
              case Left(error) =>
                Left(error)
            }
          case Right(token2) =>
            Left(PGNReaderError(token2.lineNumber, "Unexpected token"))
          case Left(error) =>
            Left(error)
        }
      case Right(token) =>
        unreadToken(token)
        Right(None)
      case Left(error) =>
        Left(error)
    }

  private def parseRatings(value: String) = {
    val ss = value.split(":")
    var isError = false
    var ratings = Vector[Option[Int]]()
    for(s <- ss if !isError) {
      s match {
        case "-" => ratings :+= None
        case _   =>
          try {
            ratings :+= Some(Integer.parseInt(s))
          } catch {
            case e: NumberFormatException =>
              isError = true
          }
      }
    }
    if(!isError) Some(ratings) else None
  }

  private def parseTimeControls(value: String) = {
    var ss = value.split(":")
    var isError = false
    var timeControls = Vector[Option[TimeControl]]()
    for(s <- ss if !isError) {
      s match {
        case "-" => timeControls :+= None
        case _   =>
          TimeControl.parseTimeControl(s) match {
            case Some(timeControl) => timeControls :+= Some(timeControl)
            case None              => isError = true
          }
      }
    }
    if(!isError) Some(timeControls) else None    
  }

  private def readAndCheckMoveStop(resultOpt: Option[Result.Value]) =
    readToken() match {
      case Right(token @ SymbolToken(symbol, _)) =>
        var errorOpt = None: Option[PGNReaderError]
        var isStop = false
        for {
          result <- resultOpt
          result2 <- stringToResultOption(symbol)
        } {
          if(result == result2) {
            isStop = true
          } else {
            errorOpt = Some(PGNReaderError(token.lineNumber, "Game termination isn't equal to result"))
          }
        }
        errorOpt match {
          case None =>
            if(!isStop) unreadToken(token)
            Right(isStop)
          case Some(error) =>
            Left(error)
        }
      case Right(token @ OtherToken('*', _)) =>
        var errorOpt = None: Option[PGNReaderError]
        var isStop = false
        for(result <- resultOpt) {
          if(result == Result.Unfinished) {
            isStop = true
          } else {
            errorOpt = Some(PGNReaderError(token.lineNumber, "Game termination isn't equal to result"))
          }
        }
        errorOpt match {
          case None =>
            if(!isStop) unreadToken(token)
            Right(isStop)
          case Some(error) =>
            Left(error)
        }
      case Right(OtherToken(')', tmpLineNumber)) =>
        if(resultOpt == None)
          Right(true)
        else
          Left(PGNReaderError(tmpLineNumber, "Unexpected right parenthesis"))
      case Right(token) =>
        unreadToken(token)
        Right(false)
      case Left(error) =>
        Left(error)
    }

  private def readMoveNumber() = {
    readToken() match {
      case Right(SymbolToken(s, tmpLineNumber)) =>
        var isError = false
        var moveNumber = 1
        try {
          moveNumber = Integer.parseInt(s)
        } catch {
          case e: NumberFormatException =>
            isError = true
        }
        if(!isError) {
          var isStop = false
          var errorOpt = None: Option[PGNReaderError]
          while(!isStop) {
            readToken() match {
              case Right(OtherToken('.', _)) => ()
              case Right(token)              =>
                unreadToken(token)
                isStop = true
              case Left(error)               =>
                isStop = true
            }
          }
          errorOpt match {
            case None        => Right((moveNumber, tmpLineNumber))
            case Some(error) => Left(error)
          }
        } else
          Left(PGNReaderError(tmpLineNumber, "Invlid move number"))
      case Right(token) =>
        Left(PGNReaderError(token.lineNumber, "Unexpected token"))
      case Left(error) =>
        Left(error)
    }
  }

  private def readMoveWithVariations(board: Board): Either[PGNReaderError, (MoveWithVariations, Int)] = {
    readToken() match {
      case Right(SymbolToken(moveStr, tmpLineNumber)) =>
        Move.parseSANMove(moveStr, board) match {
          case Some(move) =>
            var errorOpt = None: Option[PGNReaderError]
            var isStop = false
            readToken() match {
              case Right(NAGToken(_, _)) =>
                ()
              case Right(token) =>
                unreadToken(token)
              case Left(error) =>
                isStop = true
                errorOpt = Some(error)
            }
            var variations = Vector[Vector[MoveWithVariations]]()
            if(!isStop) {
              while(!isStop) {
                readToken() match {
                  case Right(OtherToken('(', _)) =>
                    readMovesWithVariations(board, None) match {
                      case Right(movesWithVariations) =>
                        variations :+= movesWithVariations
                      case Left(error) =>
                        isStop = true
                        errorOpt = Some(error)
                    }
                  case Right(token) =>
                    unreadToken(token)
                    isStop = true
                  case Left(error) =>
                    isStop = true
                    errorOpt = Some(error)
                }
              }
            }
            errorOpt match {
              case None =>
                Right((MoveWithVariations(move, variations), tmpLineNumber))
              case Some(error) =>
                Left(error)
            }
          case None =>
            Left(PGNReaderError(tmpLineNumber, "Illegal move"))
        }
      case Right(token) =>
        Left(PGNReaderError(token.lineNumber, "Unexpected token"))
      case Left(error) =>
        Left(error)
    }
  }

  private def readMovesWithVariations(board: Board, resultOpt: Option[Result.Value]): Either[PGNReaderError, Vector[MoveWithVariations]] = {
    var movesWithVariations = Vector[MoveWithVariations]()
    var tmpBoard = board
    var errorOpt = None: Option[PGNReaderError]
    var isStop = false
    while(!isStop) {
      readAndCheckMoveStop(resultOpt) match {
        case Right(isStop2) =>
          if(!isStop2) {
            readMoveNumber() match {
              case Right((moveNumber, tmpLineNumber)) =>
                if(moveNumber != tmpBoard.fullmoveNumber) {
                  isStop = true
                  errorOpt = Some(PGNReaderError(tmpLineNumber, "Move number isn't equal to board fullmove number"))
                }
              case Left(error) =>
                isStop = true
                errorOpt = Some(error)
            }
            var mustBeMoveNumber = false
            if(!isStop) {
              if(tmpBoard.side == Side.White) {
                readMoveWithVariations(tmpBoard) match {
                  case Right((moveWithVariations, tmpLineNumber)) =>
                    tmpBoard.makeMove(moveWithVariations.move) match {
                      case Some(tmpBoard2) =>
                        tmpBoard = tmpBoard2
                        movesWithVariations :+= moveWithVariations
                        mustBeMoveNumber = !moveWithVariations.variations.isEmpty
                      case None =>
                        errorOpt = Some(PGNReaderError(tmpLineNumber, "Illegal move"))
                    }
                  case Left(error) =>
                    isStop = true
                    errorOpt = Some(error)
                }
              }
            }
            if(!isStop) {
              readAndCheckMoveStop(resultOpt) match {
                case Right(isStop3) =>
                  if(!isStop3) {
                    if(mustBeMoveNumber) {
                      readMoveNumber() match {
                        case Right((moveNumber, tmpLineNumber)) =>
                          if(moveNumber != tmpBoard.fullmoveNumber) {
                            isStop = true
                            errorOpt = Some(PGNReaderError(tmpLineNumber, "Move number isn't equal to board fullmove number"))
                          }
                        case Left(error) =>
                          isStop = true
                          errorOpt = Some(error)
                      }
                    }
                    if(!isStop) {
                      readMoveWithVariations(tmpBoard) match {
                        case Right((moveWithVariations, tmpLineNumber)) =>
                          tmpBoard.makeMove(moveWithVariations.move) match {
                            case Some(tmpBoard2) =>
                              tmpBoard = tmpBoard2
                              movesWithVariations :+= moveWithVariations
                            case None =>
                              errorOpt = Some(PGNReaderError(tmpLineNumber, "Illegal move"))
                          }
                        case Left(error) =>
                          isStop = true
                          errorOpt = Some(error)
                      }
                    }
                  } else
                    isStop = isStop3
                case Left(error) =>
                  errorOpt = Some(error)
              }
            }
          } else
            isStop = isStop2
        case Left(error) =>
          isStop = true
          errorOpt = Some(error)
      }
    }
    errorOpt match {
      case None =>
        Right(movesWithVariations)
      case Some(error) =>
        Left(error)
    }
  }
  
  override def readGame() = {
    readToken() match {
      case Right(EOFToken(_)) =>
        Right(None)
      case Right(token) =>
        unreadToken(token)
        var event = ""
        var site = ""
        var date = "????.??.??"
        var round = ""
        var white = Vector("")
        var black = Vector("")
        var result = Result.Unfinished
        var eventDateOption = None: Option[String]
        var whiteEloOption = None: Option[Vector[Option[Int]]]
        var blackEloOption = None: Option[Vector[Option[Int]]]
        var whiteUSCFOption = None: Option[Vector[Option[Int]]]
        var blackUSCFOption = None: Option[Vector[Option[Int]]]
        var ecoOption = None: Option[String]
        var timeControlOption = None: Option[Vector[Option[TimeControl]]]
        var tags = Map[String, String]()
        var boardOption = None: Option[Board]
        var errorOpt = None: Option[PGNReaderError]
        var isStop = false
        while(!isStop) {
          readTag() match {
            case Right(Some((Tag(name, value), tmpLineNumber))) =>
              tags += ((name, value))
              name match {
                case "Event" =>
                  event = value
                case "Site" =>
                  site = value
                case "Date" =>
                  date = value
                case "Round" =>
                  round = value
                case "White" =>
                  white = value.split(":").toVector
                case "Black" =>
                  black = value.split(":").toVector
                case "Result" =>
                  stringToResultOption(value) match {
                    case Some(result2) => result = result2
                    case None          =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid result"))
                  }
                case "EventDate" =>
                  eventDateOption = Some(value)
                case "WhiteElo" =>
                  parseRatings(value) match {
                    case Some(ratings) => whiteEloOption = Some(ratings)
                    case None          =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid Elo"))
                  }
                case "BlackElo" =>
                  parseRatings(value) match {
                    case Some(ratings) => blackEloOption = Some(ratings)
                    case None          =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid Elo"))
                  }
                case "WhiteUSCF" =>
                  parseRatings(value) match {
                    case Some(ratings) => whiteUSCFOption = Some(ratings)
                    case None          =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid USCF"))
                  }
                case "BlackUSCF" =>
                  parseRatings(value) match {
                    case Some(ratings) => blackUSCFOption = Some(ratings)
                    case None          =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid USCF"))
                  }
                case "ECO" =>
                  ecoOption = Some(value)
                case "TimeControl" =>
                  parseTimeControls(value) match {
                    case Some(timeControls) => timeControlOption = Some(timeControls)
                    case None               =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid time control"))
                  }
                case "FEN" =>
                  Board.parseBoard(value) match {
                    case Some(board) => boardOption = Some(board)
                    case None        =>
                      isStop = true
                      errorOpt = Some(PGNReaderError(tmpLineNumber, "Invalid fen"))
                  }
                case _ =>
                  ()
              }
            case Right(None) =>
              isStop = true
            case Left(error) =>
              errorOpt = Some(error)
          }
        }
        var movesWithVariations = Vector[MoveWithVariations]()
        if(!isStop) {
          readMovesWithVariations(boardOption.getOrElse(Board.Initial), Some(result)) match {
            case Right(movesWithVariations2) =>
              movesWithVariations = movesWithVariations2
            case Left(error) =>
              errorOpt = Some(error)
          }
        }
        errorOpt match {
          case None =>
            Right(Some(Game(
                  event = event,
                  site = site,
                  date = date,
                  round = round,
                  white = white,
                  black = black,
                  result = result,
                  eventDateOption = eventDateOption,
                  whiteEloOption = whiteEloOption,
                  blackEloOption = blackEloOption,
                  whiteUSCFOption = whiteUSCFOption,
                  blackUSCFOption = blackUSCFOption,
                  ecoOption = ecoOption,
                  timeControlOption = timeControlOption,
                  tags = tags,
                  boardOption = boardOption,
                  movesWithVariations = movesWithVariations)))
          case Some(error) =>
            Left(error)
        }
      case Left(error) =>
        Left(error)
    }
  }
}
