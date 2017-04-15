
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    //variables to hold totals for all colors of pixel and its neighbors
    var redTotal = 0
    var greenTotal = 0
    var blueTotal = 0
    var alphaTotal = 0
    
    val w = src.width
    val h = src.height
    val xmin = 0
    val xmax = w - 1
    val ymin = 0
    val ymax = h - 1
    
    val xIterMin = clamp(x - radius, xmin, xmax)
    val xIterMax = clamp(x + radius, xmin, xmax)
    val yIterMin = clamp(y - radius, ymin, ymax)
    val yIterMax = clamp(y + radius, ymin, ymax)
    val pixelsToAverage = (xIterMax - xIterMin + 1)*(yIterMax - yIterMin + 1)

    // Initialize while loop counters
    var xcount = xIterMin
    var ycount = yIterMin

    while (ycount <= yIterMax) {
      while (xcount <= xIterMax) {
        // Extract RGBA of neighboring pixel, or, if neighbor outside image, nearest pixel on image
        var rgbaForPixel = src.apply(xcount, ycount)
        // extract r, g, b, a channels from RGBA and add to running totals
        redTotal += red(rgbaForPixel)
        greenTotal += green(rgbaForPixel)
        blueTotal += blue(rgbaForPixel)
        alphaTotal += alpha(rgbaForPixel)
        xcount += 1
      }
      ycount += 1
      xcount = xIterMin
    }
    // Create an RGBA value from averaged color values and return
    rgba(redTotal / pixelsToAverage, greenTotal / pixelsToAverage, blueTotal / pixelsToAverage, alphaTotal / pixelsToAverage)
  }

}
