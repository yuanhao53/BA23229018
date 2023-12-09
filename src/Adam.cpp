#include <Rcpp.h>
using namespace Rcpp;
//' @name Adam
//' @title Adam Optimization
//' @description Adam optimization algorithm
//' @param X The design matrix of size M*N
//' @param y The dependent vector of size M*1
//' @param lr The learning rate
//' @param beta1 Exponential decay rate for the first moment estimate
//' @param beta2 Exponential decay rate for the second moment estimate
//' @param epsilon Small constant to avoid division by zero
//' @param num_iters The number of iterations
//' @return The vector of optimal parameter values
//' @examples
//' \dontrun{
//' X <- matrix(runif(1000), ncol = 20)
//' y <- 2*rbinom(50,1,0.5)-1
//' lr <- 0.001
//' beta1 <- 0.9
//' beta2 <- 0.999
//' epsilon <- 1e-8
//' num_iters <- 100
//' theta_adam <- adam_optimization(X, y, lr, beta1, beta2, epsilon, num_iters)
//' }
//' @export
 //[[Rcpp::export]]
 NumericVector adam_optimization(NumericMatrix X, NumericVector y, double lr, double beta1, double beta2, double epsilon, int num_iters){
   int M = X.rows(); // num of samples
   int N = X.ncol(); // num of features
   
   NumericVector theta(N, 0.0); // Define the parameter
   NumericVector m(N, 0.0); // Initialize the first moment vector
   NumericVector v(N, 0.0); // Initialize the second moment vector
   double beta1_t = 1.0; // Initialize the exponential decay factor for the first moment
   double beta2_t = 1.0; // Initialize the exponential decay factor for the second moment
   
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
       
       // Update biased first moment estimate
       m = beta1 * m + (1 - beta1) * gradient;
       // Update biased second moment estimate
       NumericVector gradient_pow(N);
       for(int j = 0; j < N; j++){
         gradient_pow[j] = pow(gradient[j], 2);
       }
       v = beta2 * v + (1 - beta2) * gradient_pow;
       
       // Correct bias in first moment
       NumericVector m_hat = m / (1 - beta1*beta1_t);
       
       // Correct bias in second moment
       NumericVector v_hat = v / (1 - beta2*beta2_t);
       
       // Update parameters
       NumericVector Step(N);
       for(int j = 0; j < N; j++){
         Step[j] = m_hat[j]/(sqrt(v_hat[j]) + epsilon);
       }
       theta = theta - lr * Step;
       
       // Update decay factors
       beta1_t = beta1_t * beta1;
       beta2_t = beta2_t * beta2;
     }
   }
   return theta;
 }