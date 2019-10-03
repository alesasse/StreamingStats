package it.datasoil.streamingstats

import breeze.linalg._
import scala.math.sqrt

class Extrema {

  var count: Int = 0
  var min: Double = Double.PositiveInfinity
  var max: Double = Double.NegativeInfinity

  def clear: Unit = {
    count = 0
    min = Double.PositiveInfinity
    max = Double.NegativeInfinity
  }

  def fit(x: Double): Unit = {

    if (x.isNaN) return

    count += 1

    if (x < min){
      min = x
    }
    if (x > max){
      max = x
    }
  }

  def show: Unit = {
    println(s"Extrema: n=${count} | min = ${min}, max = ${max}")
  }

}


// Welford's online algorithm: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
class Variance(wf: WeightFunction){

  var count = 0
  var mean = Double.NaN
  var M2 = Double.NaN
  val weight = wf

  def this(){
    this(new EqualWeight())
  }

  def fit(x: Double): Unit ={
    count += 1
    val reductionFactor = weight.getWeight()
    if (count==1) {
      mean = x
      M2 = 0.0
      return
    }
//    val reductionFactor = weight.getWeight()
    val delta = x - mean
    val newmean = mean*(1.0-reductionFactor) + reductionFactor*x
    val delta2 = x - newmean
    M2 += delta * delta2
    mean = newmean
    return
  }

  def value(): Double = {
    M2/(count-1)
  }

  def show = {
    println(s"Variance: n=${count} | var = ${this.value()}")
  }

}


class Mean(wf: WeightFunction) {

  var count = 0
  var mean = Double.NaN
  val weight = wf

  def this(){
    this(new EqualWeight())
  }

  def value(): Double ={
    mean
  }


  def clear = {
    count = 0
    mean = 0.0
  }

  def fit(x: Double): Unit ={
    if (x.isNaN) return
    count += 1
    val reductionFactor = weight.getWeight()
    if (count == 1) {
      mean = x
    } else {
      mean = mean*(1-reductionFactor) + reductionFactor*x
    }
  }

  def show = {
    println(s"Mean: n=${count} | mean = ${this.value()}")
  }
}


class CovMatrix(nFeatures: Int, wf: WeightFunction){

//  var value = DenseMatrix.zeros[Double](nFeatures,nFeatures)
  var A = DenseMatrix.zeros[Double](nFeatures,nFeatures)
  var b = DenseVector.zeros[Double](nFeatures)
  var weight = wf
  var count = 0

  def this(p: Int){
    this(p, new EqualWeight())
  }

  def fit(x: DenseVector[Double]): Unit ={
    count += 1
    val reductionFactor = weight.getWeight()
    // smooth!(b, x, gamma)
    for (i <- 0 until b.length){
      b(i) = b(i)*(1-reductionFactor) + reductionFactor*x(i)
    }
    // smooth_syr!(o.A, x, γ)
    for (j <- 0 until A.cols){
      for (i <- 0 to j){
        A(i,j) = A(i,j)*(1-reductionFactor) + reductionFactor * x(i) * x(j)
      }
    }
  }

  def fit(x: Array[Double]): Unit ={
    this.fit(new DenseVector[Double](x))
  }

  def value(): DenseMatrix[Double] ={
    val tmp = A - b * b.t
    val hermit = upperTriangular(tmp) + upperTriangular(tmp).t - diag(diag(tmp))
    return (count.toDouble/(count -1))*hermit
  }

  def corr(): DenseMatrix[Double] = {
    var v = diag(value())
    for (i <- 0 until v.length){
      v(i) = 1.0 / sqrt(v(i))
    }
    return diag(v) * value() * diag(v)
  }


}
