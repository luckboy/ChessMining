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
package pl.luckboy.chessmining.value
import java.io._
import pl.luckboy.chessmining.chess._

/** Represents a board network that contains two graphs. The edges of two graphs represent the 
  * relationships between pieces and squares for two sides. An edge contains the number of edges. 
  *
  * @param edgeCounts the numbers of the edges. 
  */
case class BoardNetwork(edgeCounts: Array[Array[Array[Long]]])
{
  /** Returns the number of edges.
    *
    * @param side the side.
    * @param coloredPiece1 the first colored piece.
    * @param squ1 the first square.
    * @param coloredPiece2 the second colored piece.
    * @param squ2 the second square.
    * @return the number of edges.
    */
  def edgeCount(side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int) = {
    val coloredPieceIdx1 = coloredPieceToIndex(coloredPiece1)
    val coloredPieceIdx2 = coloredPieceToIndex(coloredPiece2)
    edgeCounts(side.id)(coloredPieceIdx1 * 64 + squ1)(coloredPieceIdx2 *64 + squ2)
  }

  /** Returns the number of edges.
    *
    * @param side the side.
    * @param node1 the first node.
    * @param node2 the second node.
    * @return the number of edges.
    */
  def edgeCount(side: Side.Value, node1: BoardNetworkNode, node2: BoardNetworkNode): Long =
    edgeCount(side, node1.coloredPiece, node1.square, node2.coloredPiece, node2.square)
    
  /** Adds the value to the number of edges.
    *
    * @param side the side.
    * @param coloredPiece1 the first colored piece.
    * @param squ1 the first square.
    * @param coloredPiece2 the second colored piece.
    * @param squ2 the second square.
    * @param value the value.
    */
  def addToEdgeCount(side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int, value: Long)
  {
    val coloredPieceIdx1 = coloredPieceToIndex(coloredPiece1)
    val coloredPieceIdx2 = coloredPieceToIndex(coloredPiece2)
    edgeCounts(side.id)(coloredPieceIdx1 * 64 + squ1)(coloredPieceIdx2 *64 + squ2) += value
  }

  /** Adds the value to the number of edges.
    *
    * @param side the side.
    * @param node1 the first node.
    * @param node2 the second node.
    * @param value the value.
    */
  def addToEdgeCount(side: Side.Value, node1: BoardNetworkNode, node2: BoardNetworkNode, value: Long)
  {
    addToEdgeCount(side, node1.coloredPiece, node1.square, node2.coloredPiece, node2.square, value)
  }

  /** Folds the edges.
    *
    * @tparam T the result type.
    * @param side the side.
    * @param z the start value.
    * @param f the function.
    * @return a result of folded edges. 
    */
  def foldEdges[T](side: Side.Value, z: T)(f: (T, BoardNetworkEdge) => T) =
    (0 until (13 * 64)).foldLeft(z) {
      (x: T, y: Int) =>
        val node1 = BoardNetworkNode(indexToColoredPiece(y / 64), y % 64)
        (0 until (13 * 64)).foldLeft(x) {
          (x2: T, y2: Int) =>
            val node2 = BoardNetworkNode(indexToColoredPiece(y2 / 64), y2 % 64)
            val edge = BoardNetworkEdge(node1, edgeCounts(side.id)(y)(y2), node2)
            f(x2, edge)
        }
    }

  /** Filters the edges.
    *
    * @param side the side.
    * @param f the function.
    * @return a filtered edges.
    */
  def filterEdges(side: Side.Value)(f: (BoardNetworkEdge) => Boolean) =
    foldEdges(side, Vector[BoardNetworkEdge]()) {
      (edges: Vector[BoardNetworkEdge], edge: BoardNetworkEdge) =>
        if(f(edge)) edges :+ edge else edges
    }

  /** Saves this board network.
    *
    * @param fileName the file name.
    */
  def save(fileName: String)
  {
    save(new File(fileName))
  }
  
  /** Saves this board network.
    *
    * @param file the file.
    */
  def save(file: File)
  {
    val bos = new BufferedOutputStream(new FileOutputStream(file))
    val bnw = new BoardNetworkWriter(new OutputStreamWriter(bos, "UTF-8"))
    try {
      bnw.writeBoardNetwork(this)
    } finally {
      bnw.close()
    }
  }
}

object BoardNetwork
{
  /** Creates a new board network.
    *
    * @return a new board network.
    */
  def apply(): BoardNetwork = {
    val edgeCounts = Array.fill[Array[Array[Long]]](2)(null)
    for(sideId <- 0 until 2) {
      edgeCounts(sideId) = Array.fill[Array[Long]](13 * 64)(null)
      for(idx <- 0 until (13 * 64)) {
        edgeCounts(sideId)(idx) = Array.fill(13 * 64)(0L)
      }
    }
    BoardNetwork(edgeCounts)
  }
  
  /** Loads a board network.
    *
    * @param fileName the file name.
    * @return an optional board network.
    */
  def load(fileName: String): Option[BoardNetwork] = load(new File(fileName))
  
  /** Loads a board network.
    *
    * @param file the file.
    * @return an optional board network.
    */
  def load(file: File) = {
    val bis = new BufferedInputStream(new FileInputStream(file))
    val bnr = new BoardNetworkReader(new InputStreamReader(bis, "UTF-8"))
    try {
      bnr.readBoardNetwork()
    } finally {
      bnr.close()
    }
  }
}
