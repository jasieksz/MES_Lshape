import org.jzy3d.plot3d.builder.Mapper

class RoundMapper(valuesArr: Array[Double], size: Int) extends Mapper {

  private val gridSize = 1.0
  private val cellSize = gridSize / size

  override def f(x: Double, y: Double) = {
    val valX = (x / cellSize).round.toInt
    val valY = (y / cellSize).round.toInt
    val pos = size + valX + (-valY + size) * (2 * size + 1)
    valuesArr(pos)
  }

}
