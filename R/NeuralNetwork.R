#' @title A training process of NeuralNetwork
#' @description A training process of NeuralNetwork, including BP algorithm of a simple MLP model
#' @param X the feature dataset
#' @param y the target label
#' @param hidden_size the width of hidden layer
#' @param lr learning rate
#' @param epochs number of epochs to be trained
#' @param lambda regularization coefficient
#' @return The loss graph
#' @examples
#' \dontrun{
#' train_inputs <- matrix(runif(1000), ncol = 20, byrow = TRUE)
#' train_targets <- 2*rbinom(50, 1, 0.5)-1
#' mlp_train(train_inputs, train_targets, hidden_size = 8, lr = 0.001, epochs = 50, lambda = 0.001)
#' }
#' @export
mlp_train <- function(X, y, hidden_size, lr, epochs, lambda = 0.01) {
  input_size <- ncol(X)
  output_size <- 1
  loss_history <- c()
  # Init
  weights_input_hidden <- matrix(runif(input_size * hidden_size), nrow = input_size, ncol = hidden_size)
  biases_hidden <- matrix(runif(hidden_size), nrow = 1, ncol = hidden_size)
  
  weights_hidden_output <- matrix(runif(hidden_size * output_size), nrow = hidden_size, ncol = output_size)
  biases_output <- matrix(runif(output_size), nrow = 1, ncol = output_size)
  
  # forward
  forward <- function(inputs) {
    hidden_input <- inputs %*% weights_input_hidden + biases_hidden
    hidden_output <- sigmoid(hidden_input)
    
    final_input <- hidden_output %*% weights_hidden_output + biases_output
    final_output <- sigmoid(final_input)
    
    return(list(hidden_output = hidden_output, final_output = final_output))
  }
  
  # MSELoss
  calculate_loss <- function(predicted, target) {
    loss <- sum((predicted - target)^2) / 2
    return(loss)
  }
  
  # BP
  backward <- function(inputs, target, hidden_output, final_output) {
    # output layer
    output_error <- target - final_output
    output_delta <- output_error * sigmoid_derivative(final_output)
    
    # hidden layer
    hidden_error <- output_delta %*% t(weights_hidden_output)
    hidden_delta <- hidden_error * sigmoid_derivative(hidden_output)
    
    # update
    weights_hidden_output <<- weights_hidden_output + (t(hidden_output) %*% output_delta - lambda * weights_hidden_output) * lr
    biases_output <<- biases_output + output_delta * lr
    
    weights_input_hidden <<- weights_input_hidden + (t(inputs) %*% hidden_delta - lambda * weights_input_hidden) * lr
    biases_hidden <<- biases_hidden + hidden_delta * lr
  }
  # training model
  for (epoch in 1:epochs) {
    for (i in 1:nrow(X)) {
      input_data <- matrix(X[i, ], nrow = 1)
      target_data <- matrix(y[i], nrow = 1)
      
      # forward process
      outputs <- forward(input_data)
      
      # calculate loss
      loss <- calculate_loss(outputs$final_output, target_data)
      
      # BP
      backward(input_data, target_data, outputs$hidden_output, outputs$final_output)
      
    }
    # hold loss for every epoch
    loss_history <- c(loss_history, loss)
  }
  plot(loss_history, type = 'l', main = 'Loss Over Time', xlab = 'Iteration', ylab = 'Loss')
}