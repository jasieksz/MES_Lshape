import org.jzy3d.analysis.{AbstractAnalysis, AnalysisLauncher}
import org.jzy3d.chart.factories.AWTChartComponentFactory
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.colors.{Color, ColorMapper}
import org.jzy3d.maths.Range
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.builder.concrete.OrthonormalGrid
import org.jzy3d.plot3d.rendering.canvas.Quality

object Main extends App {

  AnalysisLauncher.open(new MyAnalysis)

}

class MyAnalysis extends AbstractAnalysis {

  override def init() = {
    val size = 20
    val result = new MesSolver().solve(size)
    val mapper = new RoundMapper(result, size)

    val range = new Range(-1, 1)
    val steps = 2 * size + 1

    val surface = Builder.buildOrthonormal(new OrthonormalGrid(range, steps, range, steps), mapper)
    surface.setColorMapper(new ColorMapper(new ColorMapRainbow, surface.getBounds.getZmin, surface.getBounds.getZmax, new Color(1, 1, 1, .5f)))
    surface.setFaceDisplayed(true)
    surface.setWireframeDisplayed(true)

    chart = AWTChartComponentFactory.chart(Quality.Advanced, getCanvasType)
    chart.getScene.getGraph.add(surface)
  }

}
