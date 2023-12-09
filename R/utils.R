# Activate Function
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Derivative of Activate Function
sigmoid_derivative <- function(x) {
  return(x * (1 - x))
}



