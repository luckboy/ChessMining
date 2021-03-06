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
import java.io._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.io._
import pl.luckboy.chessmining.iterator._
import pl.luckboy.chessmining.ui._

/** This object provides methods which creates an iterator of games.
  *
  * The example usages are:
  * {{{
  * val games = Games.fromFile("somefile.pgn")
  *
  * val games = Games.fromDirectory("somedirectory")
  * }}}
  */
object Games
{
  /** Creates an interator of games from the file.
    *
    * @param fileName the file name.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFile(fileName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): GameReaderIterator =
    fromFile(fileName, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the file.
    *
    * @param fileName the file name.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFile(fileName: String, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): GameReaderIterator =
    fromFile(new File(fileName), showVar)

  /** Creates an interator of games from the file.
    *
    * @param file the file.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFile(file: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): GameReaderIterator=
    fromFile(file, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the file.
    *
    * @param file the file.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFile(file: File, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory) = {
    val fpb = fileProgressBarFactory(file.getPath(), file.length())
    new GameReaderIterator({
      () =>
        val bis = new BufferedInputStream(new FileInputStream(file))
        val fpbis = new FileProgressBarInputStream(bis, fpb)
        gameReaderFactory(fpbis)
    }, fpb, showVar)
  }

  /** Creates an interator of games from the directory.
    *
    * @param dirName the directory name.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromDirectory(dirName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromDirectory(dirName, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the directory.
    *
    * @param dirName the directory name.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromDirectory(dirName: String, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromDirectory(new File(dirName), showVar)

  /** Creates an interator of games from the directory.
    *
    * @param dir the directory.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromDirectory(dir: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromDirectory(dir, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the directory.
    *
    * @param dir the directory.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromDirectory(dir: File, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] = {
    val files = dir.listFiles()
    if(files != null)
      files.sorted.foldLeft(Iterator.empty: Iterator[Game]) {
        (iter: Iterator[Game], file: File) => iter ++ fromFileOrDirectory(file, showVar)
      }
    else
      Iterator.empty
  }

  /** Creates an interator of games from the file or the directory.
    *
    * @param fileOrDirName the file name or the directory name.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFileOrDirectory(fileOrDirName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromFileOrDirectory(fileOrDirName, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the file or the directory.
    *
    * @param fileOrDirName the file name or the directory name.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFileOrDirectory(fileOrDirName: String, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromFileOrDirectory(new File(fileOrDirName), showVar)

  /** Creates an interator of games from the file or the directory.
    *
    * @param fileOrDir the file or the directory.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFileOrDirectory(fileOrDir: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromFileOrDirectory(fileOrDir, ShowVariable(ShowFlag.Auto))

  /** Creates an interator of games with the show variable from the file or the directory.
    *
    * @param fileOrDir the file or the directory.
    * @param showVar the show variable.
    * @param gameReaderFactory the factory of game reader.
    * @param fileProgressBarFactory the factory of file progress bar.
    * @return an interator of games.
    */
  def fromFileOrDirectory(fileOrDir: File, showVar: ShowVariable)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    if(fileOrDir.isDirectory())
      fromDirectory(fileOrDir, showVar)
    else
      fromFile(fileOrDir, showVar)
}
