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

object Games
{
  def fromFile(fileName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromFile(new File(fileName))

  def fromFile(file: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory) = {
    val fpb = fileProgressBarFactory(file.getPath(), file.length())
    val bis = new BufferedInputStream(new FileInputStream(file))
    val fpbis = new FileProgressBarInputStream(bis, fpb)
    new GameReaderIterator(gameReaderFactory(fpbis), fpb)
  }

  def fromDirectory(dirName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromDirectory(new File(dirName))

  def fromDirectory(dir: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] = {
    val files = dir.listFiles()
    if(files != null)
      files.sorted.foldLeft(Iterator.empty: Iterator[Game]) {
        (iter: Iterator[Game], file: File) => iter ++ fromFileOrDirectory(file)
      }
    else
      Iterator.empty
  }

  def fromFileOrDirectory(fileOrDirName: String)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    fromFileOrDirectory(new File(fileOrDirName))

  def fromFileOrDirectory(fileOrDir: File)(implicit gameReaderFactory: GameReaderFactory, fileProgressBarFactory: FileProgressBarFactory): Iterator[Game] =
    if(fileOrDir.isDirectory())
      fromDirectory(fileOrDir)
    else
      fromFile(fileOrDir)
}
