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
package pl.luckboy.chessmining.chart
import scala.collection.GenTraversableOnce
import scalax.chart.module._
import pl.luckboy.chessmining.data._
import language.higherKinds

trait BoardDatasetConversions extends Converting with RichChartingCollections
{
  abstract class ToBoardDataset[T] protected () extends Converter[T]
  {
    type X <: BoardDataset
  }
  
  object ToBoardDataset extends ConverterCompanion[BoardDataset, ToBoardDataset]
  {
    final def apply[T, U <: BoardDataset](f: T => U): ToBoardDataset[T] = new ToBoardDataset[T] {
      override final type X = U

      override final def convert(a: T): X = f(a)
    }

    implicit def FromTuple2s[T, U: Numeric, VV[X] <: GenTraversableOnce[X]](implicit evT: T => Comparable[T]): ToBoardDataset[VV[(T, Array[U])]] = apply {
      (data: VV[(T, Array[U])]) =>
        val dataset = new DefaultBoardDataset()
        data foreach {
          case ((seriesKey, series)) =>
            dataset.addSeries(seriesKey, series.map { implicitly[Numeric[U]].toDouble(_) }) 
        }
        dataset
    }
  }
}

object BoardDatasetConversions extends BoardDatasetConversions
