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
import java.awt._
import java.awt.geom._
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.data.general._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.data._

class BoardPlot(ds: BoardDataset) extends Plot
{
  private var dataset = ds
  if(dataset != null) {
    setDatasetGroup(dataset.getGroup())
    dataset.addChangeListener(this)
  }
  private var interiorGap: Double = BoardPlot.DefaultInteriorGap
  private var fontGap: Double = BoardPlot.DefaultFontGap
  private var font: Font = BoardPlot.DefaultFont
  private var paint: Paint = BoardPlot.DefaultPaint
  private var stroke: Stroke = BoardPlot.DefaultStroke
  private var maxColumnCount = BoardPlot.DefaultMaxColumnCount
  private var seriesPaint: Paint = null
  private var seriesPaintMap: PaintMap = new PaintMap()
  private var seriesBasePaint: Paint = java.awt.Color.gray
  private var hasAutoPopulateSeriesPaint = true

  def getDataset() = dataset
  
  def setDataset(dataset: BoardDataset)
  {
    if(this.dataset != null) this.dataset.removeChangeListener(this)
    this.dataset = dataset
    if(this.dataset != null) {
      setDatasetGroup(this.dataset.getGroup())
      this.dataset.addChangeListener(this)
    }
    datasetChanged(new DatasetChangeEvent(this, dataset))
  }

  def getInteriorGap() = interiorGap

  def setInteriorGap(interiorGap: Double)
  {
    this.interiorGap = interiorGap
    fireChangeEvent()
  }

  def getFontGap() = fontGap

  def setFontGap(fontGap: Double)
  {
    this.fontGap = fontGap
    fireChangeEvent()
  }

  def getFont() = font

  def setFont(font: Font)
  {
    this.font = font
    fireChangeEvent()
  }

  def getPaint() = paint

  def setPaint(paint: Paint)
  {
    this.paint = paint
    fireChangeEvent()
  }

  def getStroke() = stroke

  def setStroke(stroke: Stroke)
  {
    this.stroke = stroke
    fireChangeEvent()
  }

  def getMaxColumnCount() = maxColumnCount
  
  def setMaxColumnCount(max: Int)
  {
    this.maxColumnCount = max
    fireChangeEvent()
  }
  
  def getSeriesPaint() = seriesPaint

  def setSeriesPaint(paint: Paint)
  {
    this.seriesPaint = paint
    fireChangeEvent()
  }

  def getSeriesBasePaint() = seriesBasePaint

  def setSeriesBasePaint(paint: Paint)
  {
    this.seriesBasePaint = paint
    fireChangeEvent()
  }
  
  protected def lookupSeriesPaint(seriesKey: Comparable[_]) = {
    if(seriesPaint != null) {
      seriesPaint
    } else {
      var result = seriesPaintMap.getPaint(seriesKey)
      if(result != null) {
        result
      } else {
        if(hasAutoPopulateSeriesPaint) {
          val ds = getDrawingSupplier()
          if(ds != null) {
            result = ds.getNextPaint()
            seriesPaintMap.put(seriesKey, result)
            result
          } else
            seriesBasePaint
        } else
          seriesBasePaint
      }
    }
  }

  def getAutoPopulateSeriesPaint() = hasAutoPopulateSeriesPaint
  
  def setAutoPopulateSeriesPaint(auto: Boolean)
  {
    hasAutoPopulateSeriesPaint = auto
    fireChangeEvent()
  }

  override def getPlotType() = "Board_Plot"

  override def draw(g2: Graphics2D, area: Rectangle2D, anchor: Point2D, parentState: PlotState, info: PlotRenderingInfo)
  {
    val insets = getInsets()
    insets.trim(area)
    if(info != null) {
      info.setPlotArea(area)
      info.setDataArea(area) 
    }
    drawBackground(g2, area)
    drawOutline(g2, area)
    val savedClip = g2.getClip()
    g2.clip(area)
    if(!(dataset == null || dataset.getSeriesCount() == 0)) {
      val savedFont = g2.getFont()
      val savedPaint = g2.getPaint()
      val savedStroke = g2.getStroke()
      g2.setFont(font)
      g2.setPaint(paint)
      g2.setStroke(stroke)
      val boardCounts = Array.fill(64)(0.0)
      for(squ <- 0 until 64) {
        boardCounts(squ) = 0
        for(i <- 0 until dataset.getSeriesCount()) {
          boardCounts(squ) += dataset.getValue(i, squ)
        }
      }
      val colCount = dataset.getSeriesCount().min(maxColumnCount)
      val rowCount = (dataset.getSeriesCount() + colCount - 1) / colCount
      val widthGap = area.getWidth() * interiorGap
      val heightGap = area.getHeight() * interiorGap
      val width = (area.getWidth() - widthGap) / colCount.toDouble - widthGap
      val height = (area.getHeight() - heightGap) / rowCount.toDouble - heightGap
      val boardWidth = width.min(height)
      val startX = area.getX() + (area.getWidth() - ((boardWidth + widthGap) * colCount.toDouble - widthGap)) / 2.0
      val startY = area.getY() + (area.getHeight() - ((boardWidth + heightGap) * rowCount.toDouble - heightGap)) / 2.0
      for(i <- (0 until dataset.getSeriesCount())) {
        val col = i % colCount
        val row = i / colCount
        val x = startX + col.toDouble * (boardWidth + widthGap)
        val y = startY + row.toDouble * (boardWidth + heightGap)
        drawBoard(g2, dataset.getSeriesKey(i), boardCounts, x, y, boardWidth)
      }
      g2.setStroke(savedStroke)
      g2.setPaint(savedPaint)
      g2.setFont(savedFont)
    } else
      drawNoDataMessage(g2, area)
    g2.setClip(savedClip)
    drawOutline(g2, area)
  }

  protected def drawBoard(g2: Graphics2D, seriesKey: Comparable[_], boardCounts: Array[Double], x: Double, y: Double, width: Double)
  {
    val fontMetrics = g2.getFontMetrics()
    var maxFontWidth =  ((0 until 8).map(columnToChar _) ++ (0 until 8).map(rowToChar _)).map(fontMetrics.charWidth _).max.toDouble
    val tmpWidth = maxFontWidth * (1.0 + fontGap)
    val tmpHeight = fontMetrics.getHeight().toDouble * (1.0 + fontGap)
    val fontWidthWithGap = tmpWidth.max(tmpHeight)
    val boardWidth = width - fontWidthWithGap * 2.0
    val boardX = x + fontWidthWithGap
    val boardY = y + fontWidthWithGap
    for(row <- 7 to 0 by -1) {
      val squY = boardY + (7 - row).toDouble * boardWidth / 8.0
      for(col <- 0 to 7) {
        val squ = col + (row << 3)
        val squX = boardX + col.toDouble * boardWidth / 8.0
        val squRect = new Rectangle2D.Double(squX, squY, boardWidth / 8.0, boardWidth / 8.0)
        val savedPaint = g2.getPaint()
        val savedComposite = g2.getComposite()
        g2.setPaint(java.awt.Color.white)
        g2.fill(squRect)
        val seriesIdx = dataset.indexOf(seriesKey)
        val alphaComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, (dataset.getValue(seriesIdx, squ) / boardCounts(squ)).min(1.0).toFloat)
        g2.setPaint(lookupSeriesPaint(seriesKey))
        g2.setComposite(alphaComposite)
        g2.fill(squRect)
        g2.setComposite(savedComposite)
        g2.setPaint(savedPaint)
      }
    }
    val boardRect = new Rectangle2D.Double(boardX, boardY, boardWidth, boardWidth)
    g2.draw(boardRect)
    for(col <- 1 to 7) {
      val lineX = boardX + col.toDouble * boardWidth / 8.0
      val line = new Line2D.Double(lineX, boardY, lineX, boardY + boardWidth)
      g2.draw(line)
    }
    for(row <- 6 to 0 by -1) {
      val lineY = boardY + (7 - row).toDouble * boardWidth / 8.0
      val line = new Line2D.Double(boardX, lineY, boardX + boardWidth, lineY)
      g2.draw(line)
    }
    for(col <- 0 to 7) {
      val c = columnToChar(col)
      val charWidth = fontMetrics.charWidth(c).toDouble
      val colX = (boardX + col.toDouble * boardWidth / 8.0 + (boardWidth / 8.0 - charWidth) / 2.0).toFloat
      val baselineY = fontMetrics.getAscent().toDouble
      val colY1 = (y + baselineY).toFloat
      val colY2 = (boardY + boardWidth + fontWidthWithGap + baselineY - fontMetrics.getHeight().toDouble).toFloat
      g2.drawString(c.toString(), colX, colY1)
      g2.drawString(c.toString(), colX, colY2)
    }
    for(row <- 7 to 0 by -1) {
      val c = rowToChar(row)
      val charWidth = fontMetrics.charWidth(c).toDouble
      val rowX1 = (x + (maxFontWidth - charWidth) / 2.0).toFloat
      val rowX2 = (boardX + boardWidth + fontWidthWithGap - maxFontWidth + (maxFontWidth - charWidth) / 2.0).toFloat
      val baselineY = fontMetrics.getAscent().toDouble
      val rowY = (boardY + ((7 - row).toDouble * boardWidth / 8.0) + (boardWidth / 8.0 - fontMetrics.getHeight()) / 2.0 + baselineY).toFloat
      g2.drawString(c.toString(), rowX1, rowY)
      g2.drawString(c.toString(), rowX2, rowY)
    }
  }

  override def getLegendItems() = {
    val legendItems = new LegendItemCollection()
    if(dataset != null) {
      for(i <- 0 until dataset.getSeriesCount()) {
        val seriesKey = dataset.getSeriesKey(i)
        val label = seriesKey.toString()
        val desc = seriesKey.toString()
        val legendItem = new LegendItem(label, desc, null, null,
          true, Plot.DEFAULT_LEGEND_ITEM_BOX, true, lookupSeriesPaint(seriesKey), true, getOutlinePaint(), 
          getOutlineStroke(), false, new Line2D.Float(), new BasicStroke(), java.awt.Color.black)
        legendItems.add(legendItem)
      }
    }
    legendItems
  }
}

object BoardPlot
{
  val DefaultInteriorGap: Double = 0.08
  val DefaultFontGap: Double = 0.08
  val DefaultFont: Font = new Font("SansSerif", Font.PLAIN, 10)
  val DefaultPaint: Paint = java.awt.Color.black
  val DefaultStroke: Stroke = new BasicStroke(0.5f)
  val DefaultMaxColumnCount = 4
}
