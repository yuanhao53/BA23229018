#include <Rcpp.h>
using namespace Rcpp;
//' @name MinibatchGD
//' @title Mini-Batch Gradient Descent
//' @description Mini-Batch Gradient Descent optimization algorithm
//' @param X The design matrix of size M*N
//' @param y The dependent vector of size M*1
//' @param lr The learning rate
//' @param num_iters The number of iterations
//' @param batch_size The size of mini-batch
//' @return The vector of optimal parameter values
//' @examples
//' \dontrun{
//' X <- matrix(runif(1000), ncol = 20)
//' y <- 2*rbinom(50,1,0.5)-1
//' lr <- 0.0001
//' num_iters <- 100
//' batch_size <- 10
//' theta_minibatch <- minibatch_gradient_descent(X, y, lr, num_iters, batch_size)
//' }
//' @export
//[[Rcpp::export]]
 NumericVector minibatch_gradient_descent(NumericMatrix X, NumericVector y, double lr, int num_iters, int batch_size){
   int M = X.rows(); // num of samples
   int N = X.ncol(); // num of features
   
   NumericVector theta(N, 0.0); // Define the parameter
   
   for(int iter = 0; iter < num_iters; iter++){
     for(int start = 0; start < M; start += batch_size){
       int end = std::min(start + batch_size, M);
       NumericMatrix X_batch = X(Range(start, end - 1), _);
       NumericVector y_batch = y[Range(start, end - 1)];
       
       NumericVector error(batch_size, 0.0);
       for(int i = 0; i < batch_size; i++){
         double predicted = 0.0;
         for(int j = 0; j < N; j++){
           predicted += X_batch(i,j)*theta[j];
         }
         error[i] = predicted - y_batch[i];
       }
       
       NumericVector gradient(N, 0.0);
       for (int j = 0; j < N; j++) {
         for(int i = 0; i < batch_size; i++){
           gradient[j] += X_batch(i,j)*error[i];
         }
       }
       
       theta = theta - lr * gradient;
     }
   }
   return theta;
 }