package it.datasoil.streamingstats

import java.time._

//object Weights {
//
//  val EqualWeights = (x: Int) => if (x == 1) 1.0 else 1/x.toDouble
//
//  val ExponentialWeights = (b: Double)  => {(x: Int) => if (x==1) 1.0 else b}
//
//  val TemporalExponentialWeights = (b: Double, tau: Double) => {(deltaT: Double) => pow(b, deltaT/tau)}
//
//}



//trait Weights[T] {
//  def getWeight: T => Double
//}
//
//class EqualWeights() extends Weights[Int] {
//  override def getWeight = (n: Int) => if (n == 1) 1.0 else 1/n.toDouble
//}
//
//class ExponentialWeights(b: Double) extends Weights[Int] {
//  override def getWeight = (n: Int) => if (n == 1) 1.0 else b
//}
//
//class TemporalExponentialWeights(b: Double, tau: Double) extends Weights[Long]{
//  override def getWeight: Long => Double = (deltaT: Long) => pow(b, deltaT/tau)
//}

trait WeightFunction {
  def getWeight(): Double
}

class EqualWeight extends WeightFunction {
  var count = 0

  override def getWeight(): Double = {
    count += 1
    1.0 / count.toDouble
  }
}

class ExponentialWeight(b: Double) extends WeightFunction {
  var count = 0
  val base = b
  override def getWeight(): Double = {
    count += 1
    if (count == 1) 1.0 else base
  }
}

//class TimedExponentialWeight(b: Double, h: Any) extends WeightFunction {
//  override def getWeight(): Double = h match {
//    case l: Long =>
//    case t: Period =>
//    case _ => throw Exception
//  }
//}




