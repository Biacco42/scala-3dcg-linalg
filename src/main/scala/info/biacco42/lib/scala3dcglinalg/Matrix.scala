package info.biacco42.lib.scala3dcglinalg

/**
 * Created by biacco42 on 7/16/20 2020.
 *
 * @author biacco42
 */

class Matrix[Row <: Dimension, Col <: Dimension] private(elements: Seq[Float])
                                                        (implicit rowDimensionType: Row, colDimensionType: Col) {

  import scala.collection.mutable

  assert(elements.length == rowDimensionType.value * colDimensionType.value)

  private val elem: mutable.IndexedSeq[Float] = mutable.IndexedSeq(elements: _*)

  def rowDimension: Int = rowDimensionType.value

  def colDimension: Int = colDimensionType.value

  def rows: IndexedSeq[IndexedSeq[Float]] = {
    (0 until rowDimension).map { rowIndex =>
      row(rowIndex)
    }
  }

  def cols: IndexedSeq[IndexedSeq[Float]] = {
    (0 until colDimension).map { colIndex =>
      col(colIndex)
    }
  }

  def row(index: Int): IndexedSeq[Float] = {
    assert(index < rowDimension)
    elem.slice(index * colDimension, (index + 1) * colDimension).toIndexedSeq
  }

  def col(index: Int): IndexedSeq[Float] = {
    assert(index < colDimension)
    (index until elem.length by colDimension).map(elem(_))
  }

  def apply(r: Int, c: Int): Float = {
    assert(0 <= r && r < rowDimension && 0 <= c && c < colDimension)
    elem(r * colDimension + c)
  }

  def update(r: Int, c: Int, value: Float): Unit = {
    assert(0 <= r && r < rowDimension && 0 <= c && c < colDimension)
    elem(r * colDimension + c) = value
  }

  // Ops between matrices
  def +(other: Matrix[Row, Col]): Matrix[Row, Col] = {
    Matrix(elem.zip(other.elem).map { ee => ee._1 + ee._2 }.toIndexedSeq)
  }

  def -(other: Matrix[Row, Col]): Matrix[Row, Col] = {
    Matrix(elem.zip(other.elem).map { ee => ee._1 - ee._2 }.toIndexedSeq)
  }

  def *[NewCol <: Dimension](other: Matrix[Col, NewCol])(implicit newColDimension: NewCol): Matrix[Row, NewCol] = {
    val newElem = for (
      lRow <- rows;
      rCol <- other.cols
    ) yield {
      lRow.zip(rCol).foldLeft(0.0f) { (acc, pair) => acc + pair._1 * pair._2 }
    }

    Matrix[Row, NewCol](newElem)
  }

  // Ops between matrix and scalar
  def +(scalar: Float): Matrix[Row, Col] = {
    Matrix[Row, Col](elem.map(_ + scalar).toIndexedSeq)
  }

  def -(scalar: Float): Matrix[Row, Col] = {
    Matrix[Row, Col](elem.map(_ - scalar).toIndexedSeq)
  }

  def *(scalar: Float): Matrix[Row, Col] = {
    Matrix[Row, Col](elem.map(_ * scalar).toIndexedSeq)
  }

  def /(scalar: Float): Matrix[Row, Col] = {
    assert(scalar != 0.0f)
    Matrix[Row, Col](elem.map(_ / scalar).toIndexedSeq)
  }

  // Unary Ops
  def unary_+ : Matrix[Row, Col] = Matrix(this)

  def unary_- : Matrix[Row, Col] = this * -1

  def transpose(matrix: Matrix[Row, Col]): Matrix[Col, Row] = ???

  def determinant(matrix: Matrix[Row, Col])(implicit ev: Row =:= Col): Float = ???

  def inverse(matrix: Matrix[Row, Col])(implicit ev: Row =:= Col): Matrix[Row, Col] = ???
}

object Matrix {
  def apply[Row <: Dimension, Col <: Dimension](elements: Seq[Float])
                                               (implicit rowDimensionType: Row, colDimensionType: Col): Matrix[Row, Col] = {

    new Matrix(elements)
  }

  def apply[Row <: Dimension, Col <: Dimension](other: Matrix[Row, Col])
                                               (implicit rowDimensionType: Row, colDimensionType: Col): Matrix[Row, Col] = {

    Matrix(other.elem.toIndexedSeq)
  }

  def identity[Row <: Dimension, Col <: Dimension](implicit rowDimensionType: Row, colDimensionType: Col): Matrix[Row, Col] = {
    val row = rowDimensionType.value
    val col = colDimensionType.value
    val elements = Seq.tabulate(row * col)(i => if (i / col == i % col) 1.0f else 0.0f)
    new Matrix(elements)
  }

  def zeros[Row <: Dimension, Col <: Dimension](implicit rowDimensionType: Row, colDimensionType: Col): Matrix[Row, Col] = {
    val row = rowDimensionType.value
    val col = colDimensionType.value
    val elements = Seq.fill(row * col)(0.0f)
    new Matrix(elements)(rowDimensionType, colDimensionType)
  }

  def ones[Row <: Dimension, Col <: Dimension](implicit rowDimensionType: Row, colDimensionType: Col): Matrix[Row, Col] = {
    val row = rowDimensionType.value
    val col = colDimensionType.value
    val elements = Seq.fill(row * col)(0.0f)
    new Matrix(elements)(rowDimensionType, colDimensionType)
  }
}