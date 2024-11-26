#' The Mode Function
#'
#' @param x a vector of quantitative data
#'
#' @return Returns the mode of the vector that is passed as the parameter.
#' @export
#'
#' @examples
#' getmode(c(1, 3, 4, 4))

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
