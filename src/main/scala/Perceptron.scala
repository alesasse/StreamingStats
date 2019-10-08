package it.datasoil.streamingstats

import breeze.linalg._

/** Linear model, uses SGD learning algorithm for approximate inference.
  *
  * @param learningRate
  * @param nfeatures
  */
class Perceptron(val learningRate: Double, val nfeatures: Int) {
  // number of seen examples
  var n: Int = 0
  // number of features
  val p: Int = nfeatures
  // DenseVector of coefficients
  var theta = DenseVector.zeros[Double](p+1)


  def predict(X: DenseVector[Double]) = {
    val fullX = DenseVector.vertcat(DenseVector(1.0),X)
    fullX dot theta
  }

  def fit(X: DenseVector[Double], y: Double) = {
    n += 1
    val currentLoss = loss(X, y)
    val fullX = DenseVector.vertcat(DenseVector(1.0),X)
    theta = theta + fullX.map(2.0 * learningRate *  currentLoss * _)
  }

  def loss(X: DenseVector[Double], y: Double) ={
    (y - predict(X))*(y - predict(X))
  }

  def coefficients() = {
    theta
  }

  def clear(): Unit ={
    n = 0
    theta = DenseVector.fill(p+1)(0.0)
  }


}

/** Linear model. Uses an exact algorithm for inference.
  *
  * @param nfeatures
  */
class LinReg(val nfeatures: Int){

  var n: Int = 0
  var beta = DenseVector.zeros[Double](nfeatures)
  var A = DenseMatrix.zeros[Double](nfeatures+1, nfeatures+1)

  def fit(X: DenseVector[Double], y: Double) = {
    n += 1
    val gamma = 1.0/n
    for (j <- 0 until nfeatures){
      val Xj = X(j)
      A(j, nfeatures) += gamma * (Xj *y - A(j, nfeatures))
      for (i<- 0 to j){
        A(i,j) += gamma * (X(i)*Xj - A(i,j))
      }
    }
    A(nfeatures, nfeatures) += gamma * (y*y - A(nfeatures, nfeatures))
    val slicedA = A(0 to nfeatures-1, 0 to nfeatures-1)
    val symA = (upperTriangular(slicedA) + upperTriangular(slicedA).t) - diag(diag(slicedA))
    try {
      beta = symA \ A(0 to nfeatures - 1, nfeatures)
    } catch {
      case _: Throwable =>
    }
  }

  def predict(X: DenseVector[Double]) = {
    X dot beta
  }

  def coef() = {
    beta
  }

  def clear() = {
    n = 0
    beta = DenseVector.zeros[Double](nfeatures)
    A = DenseMatrix.zeros[Double](nfeatures+1, nfeatures+1)
  }

}
