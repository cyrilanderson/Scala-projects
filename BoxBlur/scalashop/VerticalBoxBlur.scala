package scalashop

import org.scalameter._
import common._


object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    val h = src.height
    var col = 0
    var row = 0
    for (col <- from to end - 1) {
      for (row <- 0 to h - 1) {
        var blurRGBA = boxBlurKernel(src, col, row, radius)
        dst.update(col, row, blurRGBA)
      }
    }

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val w = src.width
    val stripWidth = w / (Math.min(numTasks, w))
    val splitPoints = 0 to w by stripWidth
    // ensure this ends in w. This way, when last strip beloww gets put into blur, end will = w, and the last column gone over will be w - 1
    // ensures all columns are processed
    val adjSplitPoints = if (splitPoints.last ==  w) splitPoints else (splitPoints take splitPoints.last - 1) :+ w
    val stripTuples = adjSplitPoints zip adjSplitPoints.tail
    val tasks = stripTuples.map({case (from, to) => task{blur(src, dst, from, to, radius)}})
    tasks.foreach(_.join)
  }

}
