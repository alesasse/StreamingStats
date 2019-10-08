package it.datasoil.streamingstats

//import java.time._

object WeightingFunctions {

  val equalWeighting = (x: Int) => if (x == 1) 1.0 else 1/x.toDouble

  val exponentialWeighting = (b: Double)  => {(x: Int) => if (x==1) 1.0 else b}

//  val TemporalExponentialWeights = (b: Double, tau: Double) => {(deltaT: Double) => pow(b, deltaT/tau)}

}


