#' Mean and standard deviation parameterised gamma sampling function
#'
#' @param n Numeric, the number of samples to take.
#' @param mean Numeric, the mean of the distribution.
#' @param sd Numeric the standard deviation of the distribution.
#'
#' @return A vector of samples from the gamma distribution
#' @export
#'
#' @examples
#' 
#' ## Example 
#' sample <- rgamma_with_mean_sd(1000, 2, 3)
#' 
#' 
#' mean(sample) 
#' 
#' sd(sample)
#' 
#' hist(sample)
#' 
#' ## Code 
#' rgamma_with_mean_sd
rgamma_with_mean_sd <- function(n, mean, sd) {
  
  theta <- sd^2 / mean
  k <- mean / theta
  
  stats::rgamma(n, shape = k, scale = theta)
}