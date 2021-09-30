package FFT

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.util._
import scala.math._

class Complex(val re: Double, val im: Double) {
  def +(rhs: Complex): Complex = new Complex(re + rhs.re, im + rhs.im)
  def -(rhs: Complex): Complex = new Complex(re - rhs.re, im - rhs.im)
  def *(rhs: Complex): Complex = new Complex(re * rhs.re - im * rhs.im, rhs.re * im + re * rhs.im)

  // def magnitude: Double = Math.hypot(re, im)
  // Math.hypot(x, y)平方根函数
  // def phase: Double = Math.atan2(im, re)
  // Math.atan2(y, x)求角度
  override def toString: String = s"Complex($re, $im)"
  //覆盖对象的打印信息
}

// only support FFT
class FFTTest(c:myFFT) extends PeekPokeTester(c)
  with HasDataConfig
  with HasElaborateConfig {
  def fft(x: Array[Complex]): Array[Complex] = {
    require(x.length > 0 && (x.length & (x.length - 1)) == 0, "array size should be power of two")
    fft(x, 0, x.length, 1)
  }

  def fft(x: Array[Double]): Array[Complex] = fft(x.map(re => new Complex(re, 0.0)))
  def rfft(x: Array[Double]): Array[Complex] = fft(x).take(x.length / 2 + 1)
  //take函数可以过滤元素，也可以直接截取序列

//递归方法来实现DIT的方案
  private def fft(x: Array[Complex], start: Int, n: Int, stride: Int) : Array[Complex] = {
    if (n == 1) {
      return Array(x(start))
    }

    val X = fft(x, start, n / 2, 2 * stride) ++ fft(x, start + stride, n / 2, 2 * stride)
    //++用于连接两个集合

    for (k <- 0 until n / 2) {
      val even = X(k)
      val arg = -2 * Pi * k / n
      val odd = new Complex(cos(arg), sin(arg)) * X(k + n / 2)
      X(k) = even + odd
      X(k + n / 2) = even - odd
    }
    X
  }

  def reverse(a: Int, len: Int): Int = {
    var res: Int = 0
    for(i <- 0 until len) {
      res = res | (a >> i) << (len - 1 - 2 * i)
    }
    res
  }

  val r = new scala.util.Random
  var bound: Double = math.pow(2.0, BinaryPoint)
  var error: Double = 0
  var ovNum: Int = 0
  var iterNum: Int = 10

  for (t <- 0 until iterNum) {
    var a = new Array[Complex](FFTlength)
    var cnt = 0
    for (i <- 0 until FFTlength) {
      var re = -bound.toInt / 2 + r.nextInt(bound.toInt)
      var im = -bound.toInt / 2 + r.nextInt(bound.toInt)
      a(cnt) = new Complex(2 * re / bound, 2 * im / bound)
      poke(c.io.din.re, re)
      poke(c.io.din.im, im)
      poke(c.io.din_valid, 1)
      // if (i == 0) {
      //   poke(c.io.din_valid, 1)
      // } else {
      //   poke(c.io.din_valid, 0)
      // }
      step(1)
      cnt += 1
    }
    var ref = fft(a)

    var errorOne: Double = 0
    var error1: Double = 0
    var ovNum1: Int = 0
    var eps: Double = 1e-9
    for (i <- 0 until FFTlength / 2) {
      var ref1 = ref(reverse(i * 2, log2Ceil(FFTlength)))
      var d1 = peek(c.io.dout0)
      error1 = math.abs((((2 * d1("re").toDouble / bound) - ref1.re) / (ref1.re + eps) + ((2 * d1("im").toDouble / bound) - ref1.im) / (ref1.im + eps)) / 2.0)
      if (error1 <= 0.5) {
        errorOne += error1
      } else {
        ovNum1 += 1
      }
      var ref2 = ref(reverse(i * 2 + 1, log2Ceil(FFTlength)))
      var d2 = peek(c.io.dout1)
      error1 = math.abs((((2 * d2("re").toDouble / bound) - ref2.re) / (ref2.re + eps) + ((2 * d2("im").toDouble / bound) - ref2.im) / (ref2.im + eps)) / 2.0)
      if (error1 <= 0.5) {
        errorOne += error1
      } else {
        ovNum1 += 1
      }
      step(1)
    }
    errorOne = errorOne / (FFTlength - ovNum1)
    ovNum += ovNum1
    error += errorOne
    var errorOnePercent = errorOne*100
    printf("In this sample, Error rate: %.2f%% | number of ovs: %d\n", errorOnePercent, ovNum1)
  }
  error /= iterNum
  print("Total error rate is: " + error*100 + "%\n")
  print(ovNum + " of " + iterNum * FFTlength + " overflowed! " + "The overlow ratio is " + 100 * ovNum / (FFTlength * iterNum).toDouble  + "%" + "\n")
}

object myFFTTest extends App {
  iotesters.Driver.execute(args, () => new myFFT) {
    c => new FFTTest(c)
  }
}