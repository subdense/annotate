package fr.umrlastig.annotator

val rnd = new scala.util.Random(42)
object Utils {
  private def toHexString(color: (Double, Double, Double)): String = {
    val (r, g, b) = ((color._1 * 255).floor.intValue, (color._2 * 255).floor.intValue, (color._3 * 255).floor.intValue)
    f"#$r%02x$g%02x$b%02x"
  }

  // input: h in [0,360] and s,v in [0,1] - output: r,g,b in [0,1]
  private def hsl2rgb(h: Double, s: Double, l: Double) =
    val a = s * Math.min(l, 1 - l)
    def f(n: Int) =
      val k = (n + h / 30) % 12
      l - a * Math.max(Seq(k - 3, 9 - k, 1).min, -1)
    (f(0), f(8), f(4))

  def getColor: String = toHexString(hsl2rgb(rnd.nextDouble() * 360, 0.8, 0.5))
}