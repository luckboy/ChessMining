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
package pl.luckboy.chessmining.data
import java.util._
import org.jfree.data.general._

/** A default implementation of the `BoardDataset` trait. */
class DefaultBoardDataset extends AbstractSeriesDataset with BoardDataset
{
  private var seriesKeys = new ArrayList[Comparable[_]]()
  private var seriesList = new ArrayList[Array[Double]]()

  override def equals(that: Any) =
    that match {
      case dataset: DefaultBoardDataset =>
        seriesKeys == dataset.seriesKeys &&
        seriesList == dataset.seriesList
      case _                            =>
        false
    }

  override def hashCode() = seriesKeys.hashCode() ^ seriesList.hashCode()

  override def clone() = {
    val result = new DefaultBoardDataset() 
    result.seriesKeys = new ArrayList(seriesKeys)
    result.seriesList = new ArrayList(seriesList.size())
    for(i <- (0 until seriesList.size())) {
      val data = Array.fill(64)(0.0)
      Array.copy(seriesList.get(i), 0, data, 0, 64)
      result.seriesList.add(i, data)
    }
    result
  }

  override def getSeriesCount() = seriesKeys.size()

  override def getSeriesKey(series: Int) = seriesKeys.get(series)

  override def getValue(series: Int, item: Int) = seriesList.get(series)(item)
  
  /** Adds a new series.
    *
    * @param seriesKey the series key.
    * @param data the data.
    */
  def addSeries(seriesKey: Comparable[_], data: Array[Double])
  {
    val seriesIdx = indexOf(seriesKey)
    if(seriesIdx == -1) {
      seriesKeys.add(seriesKey)
      seriesList.add(data)
    } else {
      seriesList.remove(seriesIdx)
      seriesList.add(seriesIdx, data)
    }
    fireDatasetChanged()
  }
}
