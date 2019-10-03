import breeze.linalg.{DenseMatrix, DenseVector, isClose}
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalactic.TolerantNumerics

import scala.util.Random
import it.datasoil.streamingstats._
import breeze.stats._

class BaseStatsTest extends FunSuite with BeforeAndAfter {

  val extr = new Extrema


  val epsilon = 1e-4f
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(epsilon)

  test("Extrema"){
    val v = Vector.fill(100)(Random.nextDouble())
    for (el <- v){
      extr.fit(el)
    }
    assert(extr.count == 100)
    assert(v.min == extr.min, "Minimum not working")
    assert(v.max == extr.max, "Maximum not working")
  }

  test("Equally Weighted Mean and Variance"){
    val obs = DenseVector.rand(100)
    val groundTruth = meanAndVariance(obs)
    val mean = new Mean
    val sigma2 = new Variance
    for (x <- obs){
      mean.fit(x)
      sigma2.fit(x)
    }
    assert(mean.count == 100, "wrong number of obs in the mean")
    assert(sigma2.count == 100, "wrong number of obs in the variance")
    assert(mean.value() === groundTruth.mean, "mean")
    assert(sigma2.value() === groundTruth.variance, "variance")
  }

  test("Exponential Weighted Mean"){
    val base = Random.nextDouble()
    val mean = new Mean(new ExponentialWeight(base))
    var ewm = 0.0
    for (y <- 1 to 100){
      val x = Random.nextDouble()
      if (y==1) {
        ewm = x
      } else {
        ewm = x * base + ewm * (1 - base)
      }
      mean.fit(x)
    }
    assert(mean.count == 100, "wrong number of obs")
    assert(mean.value() === ewm, "wrong mean calculation")
  }

  test("Covariance matrix"){
    val cvm = new CovMatrix(2)

    val row1 = DenseVector(1.0, 2.0)
    val row2 = DenseVector(1.5, 3.0)
    cvm.fit(row1)
    cvm.fit(row2)

    assert(cvm.value() === DenseMatrix((0.125, 0.25),(0.25, 0.5)), "wrong value for covariance")
    assert(cvm.corr() === DenseMatrix((1.0, 1.0),(1.0, 1.0)), "wrong value for correlation")
  }

}
