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
