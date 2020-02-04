#' Mean and standard deviation parameterised Weibull sampling function
#'
#' @inheritParams rgamma_with_mean_sd
#' @return A vector of samples from the Weibull distribution
#' @export
#'
#' @examples
#' 
#' ## Example 
#' sample <- rweibull_with_mean_sd(1000, 3, 5)
#' 
#' mean(sample)
#' 
#' sd(sample)
#' 
#' 
#' hist(sample)
#' ## Code 
#' rweibull_with_mean_sd
rweibull_with_mean_sd <- function(n, mean, sd) {
  
  mu <- mean
  sigma <- sd
  loc <- 0
  
  ## Code adapted from the mixdist package
  cv <- sigma/(mu - loc)
  
  if (cv < 1e-06) {
    nu <- cv/(sqrt(trigamma(1)) - cv * digamma(1))
    shape <- 1/nu
    scale <- (mu - loc)/(1 + nu * digamma(1))
  }
  else {
    aa <- log(cv^2 + 1)
    nu <- 2 * cv/(1 + cv)
    repeat {
      gb <- (lgamma(1 + 2 * nu) - 2 * 
               lgamma(1 + nu) - aa) /
        (2 * (digamma(1 + 2 * nu) - digamma(1 + nu)))
      nu <- nu - gb
      if (abs(gb) < 1e-12) 
        break
    }
    shape <- 1/nu
    scale <- exp(log(mu - loc) - lgamma(1 + nu))
  }
  
  stats::rweibull(n, shape = shape, scale = scale)
}