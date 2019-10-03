import org.scalatest.FunSuite
import breeze.linalg._
import it.datasoil.streamingstats._

class PerceptronTest extends FunSuite {

  val rng = scala.util.Random

  test("New linear models always predict zero") {
    val p = rng.nextInt(10) // number of features random < 10
    val pcp = new Perceptron(0.6, p)
    val X = DenseVector.rand[Double](p)//DenseVector(rng.nextDouble)
    assert(pcp.predict(X)==0)
  }

  test("New LinReg always predicts zero") {
    val p = rng.nextInt(10) // number of features random < 10
    val lr = new LinReg(p)
    val X = DenseVector.rand[Double](p)
    assert(lr.predict(X)==0)
  }

  test("LinReg correctly fits a line") {
    //val p = rng.nextInt(10) // number of features random < 10
    val lr = new LinReg(2)
    val trueLine = (x1: Double, x2: Double) => x1 * 3.7 + 5.0 * x2
    for (i<- 1 to 5){
      val x1 = rng.nextDouble()
      val x2 = rng.nextDouble()
      val y = trueLine(x1, x2)
      lr.fit(DenseVector(x1, x2), y)
    }
    assert(isClose(lr.coef(), DenseVector(3.7, 5.0)))
  }

//  test("fitting 10000 points on a line makes the perceptron converge with tolerance 0.01"){
//
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
//    val trueLine = (x: Double) => x * 3.7 + 5.4
//    val lr = new LinReg(1)
//
//    for (i<-1 to 10000){
//      val x = rng.nextDouble
//      val y = trueLine(x)
//      lr.fit(DenseVector(x), y)
//    }
//    assert(lr.n==10000)
//    assert(DenseVector(5.4, 3.7) === lr.coef())
//  }

//  test("clear method correctly refresh parameters"){
//    val oldp = onedim.p
//    onedim.clear()
//    assert(onedim.p == oldp)
//    assert(onedim.n == 0)
//    assert(onedim.coef() == DenseVector.zeros[Double](onedim.p + 1))
//  }


//  test("fitting 10000 point on a line with noise") {
//
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.1)
//    val trueLine = (x: Double) => x * 3.7 + 5.4
//
//    for (i<-1 to 10000){
//      val x = rng.nextDouble
//      val y = trueLine(x) + rng.nextDouble()*0.01
//      onedim.fit(DenseVector(x), y)
//    }
//    assert(onedim.n==10000)
//    assert(DenseVector(5.4, 3.7).corresponds(onedim.coefficients()){_ === _})
//  }

//  test("fitting a linear 4-dim space with 10000 noisy data points"){
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.1)
//    val truePlane = (x: DenseVector[Double]) => x(0)*1.0 + x(1)*2.1 + x(2)*3.2 + x(3)*4.3 + 5.4
//
//    for (i<-1 to 10000){
//      val x = DenseVector(rng.nextDouble,rng.nextDouble,rng.nextDouble,rng.nextDouble)
//      val y = truePlane(x) + rng.nextDouble()*0.01
//      fourdim.fit(x, y)
//    }
//    assert(fourdim.n==10000)
//    assert(DenseVector(5.4, 1.0, 2.1, 3.2, 4.3).corresponds(fourdim.coefficients()){_ === _})
//  }

//  test("Fitting the car dataset"){
//    val pcp = new Perceptron(0.01, 1)
//    val bufferedSource = io.Source.fromFile("./data/car.csv")
//    /* First column is the feature, second column is the target*/
//    for (line <- bufferedSource.getLines) {
//      val cols = line.split(",").map(_.trim)
//      val x = DenseVector(cols(0).toDouble)
//      val y = cols(1).toDouble
//      // do whatever you want with the columns here
//      println(s"${x}|${y}")
//      pcp.fit(x,y)
//    }
//    bufferedSource.close
//    println(s"${pcp.coefficients()}")
//  }


}
