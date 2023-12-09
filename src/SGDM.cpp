#include <Rcpp.h>
using namespace Rcpp;
//' @name SGDM
//' @title Stochastic Gradient Descent with Momentum
//' @description Stochastic Gradient Descent optimization algorithm with momentum
//' @param X The design matrix of size M*N
//' @param y The dependent vector of size M*1
//' @param lr The learning rate
//' @param momentum The momentum parameter
//' @param num_iters The number of iterations
//' @return The vector of optimal parameter values
//' @examples
//' \dontrun{
//' X <- matrix(runif(1000), ncol = 20)
//' y <- 2*rbinom(50,1,0.5)-1
//' lr <- 0.0001
//' momentum <- 0.9
//' num_iters <- 100
//' theta_sgd_momentum <- sgd_momentum(X, y, lr, momentum, num_iters)
//' }
//' @export
//[[Rcpp::export]]
 NumericVector sgd_momentum(NumericMatrix X, NumericVector y, double lr, double momentum, int num_iters){
   int M = X.rows(); // num of samples
   int N = X.ncol(); // num of features
   
   NumericVector theta(N, 0.0); // Define the parameter
   NumericVector velocity(N, 0.0); // Define the velocity
   
   for(int iter = 0; iter < num_iters; iter++){
     for(int i = 0; i < M; i++){
       double predicted = 0.0;
       for(int j = 0; j < N; j++){
         predicted += X(i,j)*theta[j];
       }
       double error = predicted - y[i];
       
       NumericVector gradient(N, 0.0);
       for (int j = 0; j < N; j++) {
         gradient[j] = X(i,j) * error;
       }
       
       velocity = momentum * velocity - lr * gradient;
       theta = theta + velocity;
     }
   }
   return theta;
 }
