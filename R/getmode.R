#' The Mode Function
#'
#' @param x a vector of quantitative data
#'
#' @return the mode of x
#' @export
#'
#' @examples
#' getmode(c(1, 3, 4, 4))

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
