package info.biacco42.lib

/**
 * Created by biacco42 on 8/5/20 2020.
 *
 * @author biacco42
 */

package object scala3dcglinalg {
  implicit object D1 extends Dimension { override val value: Int = 1 }
  implicit object D2 extends Dimension { override val value: Int = 2 }
  implicit object D3 extends Dimension { override val value: Int = 3 }
  implicit object D4 extends Dimension { override val value: Int = 4 }

  implicit class MatrixScalar(self: Float) {
  }
}
