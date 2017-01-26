import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps

class MesSolver {

  private var length = .0
  private var halfLength = .0


  def solve(size: Int): Array[Double] = {

    val left = new DenseMatrix64F(fillLeft(size))
    val right = new DenseMatrix64F(fillRight(size))

    val X = new DenseMatrix64F(3 * size * size + 4 * size + 1, 1)

    CommonOps.solve(left, right, X)

    val t = X.getData
    val result = new Array[Double]((2 * size + 1) * (2 * size + 1))

    var poz = 0
    (0 until 2 * size + 1).foreach { i =>
      (0 until 2 * size + 1).foreach { j =>
        if (i <= size || j >= size) {
          result((2 * size + 1) * i + j) = t(poz)
          poz += 1
        } else result((2 * size + 1) * i + j) = 0
      }
    }

    //println(result.max)

    result
  }

  private def fillLeft(size: Int): Array[Array[Double]] = {

    val smallMatrix = Array(
      Array(2.0 / 3, -1.0 / 6, -1.0 / 3, -1.0 / 6),
      Array(-1.0 / 6, 2.0 / 3, -1.0 / 6, -1.0 / 3),
      Array(-1.0 / 3, -1.0 / 6, 2.0 / 3, -1.0 / 6),
      Array(-1.0 / 6, -1.0 / 3, -1.0 / 6, 2.0 / 3)
    )

    val matrixSize = 3 * size * size + 4 * size + 1
    val matrix = Array.ofDim[Double](matrixSize, matrixSize)

    val secondStart = 2 * size * size + 2 * size + 1
    val firstEnd = 2 * size * size + size

    (1 to firstEnd).foreach { i =>
      if (i % (2 * size + 1) != 0) {
        val dest = Array(i + 2 * size + 1, i + 2 * size + 2, i + 1, i)
        (0 to 3).foreach { j =>
          (0 to 3).foreach { k =>
            matrix(dest(j) - 1)(dest(k) - 1) += smallMatrix(j)(k)
          }
        }
      }
    }

    (secondStart until 3 * size * size + 3 * size).foreach { i =>
      if ((i - secondStart + 1) % (size + 1) != 0) {
        val dest = Array(i + size + 1, i + size + 2, i + 1, i)
        (0 to 3).foreach { j =>
          (0 to 3).foreach { k =>
            matrix(dest(j) - 1)(dest(k) - 1) += smallMatrix(j)(k)
          }
        }
      }
    }

    (2 * size * size + size until 2 * size * size + 2 * size + 1).foreach { i =>
      matrix.indices.foreach { j =>
        if (i == j) matrix(i)(j) = 1
        else matrix(i)(j) = 0
      }
    }

    (2 * size * size + 2 * size until matrixSize by size + 1).foreach { i =>
      matrix.indices.foreach { j =>
        if (i == j) matrix(i)(j) = 1
        else matrix(i)(j) = 0
      }
    }

    matrix
  }

  private def fillRight(size: Int): Array[Array[Double]] = {

    val vectorSize = 3 * size * size + 4 * size + 1
    length = 1.0 / size.toDouble
    halfLength = length / 2

    val vector = Array.ofDim[Double](vectorSize, 1)

    (0 until vectorSize).foreach(vector(_)(0) = 0)

    vector(0)(0) = 0.5 * g(-1, 1 - halfLength) * length + 0.5 * g(-1 + halfLength, 1) * length
    vector(2 * size)(0) = 0.5 * g(1, 1 - halfLength) * length + 0.5 * g(1 - halfLength, 1) * length
    vector(3 * size * size + 4 * size)(0) = 0.5 * g(1, -1 + halfLength) * length + 0.5 * g(1 - halfLength, -1) * length

    fillVector(1, 2 * size, 1, -1 + length, length, 1, 0, vector)
    fillVector(2 * size + 1, 2 * size * size + size, 2 * size + 1, -1, 0, 1 - length, -length, vector)
    fillVector(4 * size + 1, 2 * size * size + 3 * size, 2 * size + 1, 1, 0, 1 - length, -length, vector)
    fillVector(2 * size * size + 3 * size, 3 * size * size + 4 * size, size + 1, 1, 0, 0, -length, vector)
    fillVector(3 * size * size + 3 * size + 1, 3 * size * size + 4 * size, 1, length, length, -1, 0, vector)

    vector
  }

  private def fillVector(start: Int, end: Int, inc: Int, x: Double, incX: Double, y: Double, incY: Double, vector: Array[Array[Double]]) = {
    var px = x
    var py = y
    (start until end by inc).foreach { i =>
      vector(i)(0) = 0.5 * g(px - incX / 2, py - incY / 2) * length + 0.5 * g(px + incX / 2, py + incY / 2) * length
      px += incX
      py += incY
    }
  }

  private def g(x: Double, y: Double) = {

    val r = Math.sqrt(x * x + y * y)
    val theta = Math.atan(y / x)
    Math.pow(r, 2.0 / 3) * Math.pow(Math.sin(theta + Math.PI / 2), 2.0 / 3)
    /*
    val epsilon = 10e-7
    var result = 1.0
    if (Math.abs(x+1) < epsilon)
      result = -y
    if (Math.abs(x-1) < epsilon)
      result = y
    if (Math.abs(y+1) < epsilon)
      result = -x
    if (Math.abs(y-1) < epsilon)
      result = x
    result
    */

  }

}
