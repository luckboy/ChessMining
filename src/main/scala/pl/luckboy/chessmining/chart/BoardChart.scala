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
import org.jfree.ui.RectangleInsets
import scalax.chart._
import BoardDatasetConversions._

/** A chart of chess board. */
abstract class BoardChart extends Chart
{
  type Plot = BoardPlot

  def plot = peer.getPlot().asInstanceOf[BoardPlot]
}

/** A factory of board chart. */
object BoardChart extends ChartCompanion[BoardChart]
{ 
  override def fromPeer(jfree: JFreeChart) = {
    new BoardChart {
      override final lazy val peer = jfree
    }
  }

  /** Creates a new board chart.
    *
    * @param data the data.
    * @param theme the theme.
    * @return a new board chart.
    */
  def apply[T : ToBoardDataset](data: T)(implicit theme: ChartTheme = ChartTheme.Default): BoardChart = {
    val dataset = ToBoardDataset[T].convert(data)
    val plot = new BoardPlot(dataset)
    plot.setInsets(new RectangleInsets(0.0, 5.0, 5.0, 5.0))
    BoardChart(plot, title = "", legend = true) 
  }
}
