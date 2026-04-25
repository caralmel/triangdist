#' Triangular density
#'
#' @param x numeric vector
#' @param min lower bound
#' @param max upper bound
#' @param mode mode
#' @return density values
#' @export
dtriang <- function(x, min, max, mode) {

  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be between min and max")

  res <- numeric(length(x))

  left <- x >= min & x <= mode
  right <- x > mode & x <= max

  res[left] <- 2 * (x[left] - min) / ((max - min) * (mode - min))
  res[right] <- 2 * (max - x[right]) / ((max - min) * (max - mode))

  res
}

#' Triangular distribution function
#'
#' @param q quantiles
#' @param min lower bound
#' @param max upper bound
#' @param mode mode
#' @return probabilities
#' @export
ptriang <- function(q, min, max, mode) {

  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be between min and max")

  res <- numeric(length(q))

  left <- q >= min & q <= mode
  right <- q > mode & q <= max

  res[q < min] <- 0
  res[q > max] <- 1

  res[left] <- ((q[left] - min)^2) / ((max - min) * (mode - min))
  res[right] <- 1 - ((max - q[right])^2) / ((max - min) * (max - mode))

  res
}
