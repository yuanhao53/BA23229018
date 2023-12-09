#include <Rcpp.h>
using namespace Rcpp;
//' @name SGD
//' @title Stochastic Gradient Descent
//' @description Stochastic Gradient Descent optimization algorithm
//' @param X The design matrix of size M*N
//' @param y The dependent vector of size M*1
//' @param lr The learning rate
//' @param num_iters The number of iterations
//' @return The vector of optimal parameter values
//' @examples
//' \dontrun{
//' X <- matrix(runif(1000), ncol = 20)
//' y <- 2*rbinom(50,1,0.5)-1
//' lr <- 0.001
//' num_iters <- 100
//' theta_sgd <- stochastic_gradient_descent(X, y, lr, num_iters)
//' }
//' @export
//[[Rcpp::export]]
 NumericVector stochastic_gradient_descent(NumericMatrix X, NumericVector y, double lr, int num_iters){
   int M = X.rows(); // num of samples
   int N = X.ncol(); // num of features
   
   NumericVector theta(N, 0.0); // Define the parameter
   
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
       
       theta = theta - lr * gradient;
     }
   }
   return theta;
 }