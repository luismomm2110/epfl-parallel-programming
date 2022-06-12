package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(
        boxBlurKernel(src, x, y, 0) == rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity."
      )
  }

  test(
    "boxBlurKernel should return the correct value on  3x1 an interior pixel " +
      "of a 3x1 image with radius 1"
  ) {
    val src = new Img(3, 1)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2

    assert(
      boxBlurKernel(src, 1, 0, 1) == 1,
      s"(boxBlurKernel(1, 0, 1) should be 1, " +
        s"but it's ${boxBlurKernel(src, 1, 0, 1)})"
    )
  }

  test(
    "boxBlurKernel should return the correct value on an interior pixel in vertical image " +
      "of a 1x3 with radius 1"
  ) {
    val src = new Img(1, 3)
    src(0, 0) = 0; src(0, 1) = 1; src(0, 2) = 4

    assert(
      boxBlurKernel(src, 0, 1, 1) == 2,
      s"(boxBlurKernel(0, 1, 1) should be 2, " +
        s"but it's ${boxBlurKernel(src, 0, 1, 1)})"
    )
  }

  test(
    "boxBlurKernel should return the correct value on an interior pixel " +
      "of a 3x4 image with radius 1"
  ) {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(
      boxBlurKernel(src, 1, 2, 1) == 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})"
    )
  }
