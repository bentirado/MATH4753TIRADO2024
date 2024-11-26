#' The myci function. Creates a confidence interval of the sample mean.
#'
#' @param d a vector sample.
#' @param confInt the confidence interval. from 0 - 1
#'
#' @return A confidence interval of the sample mean.
#' @export
#'
#' @examples
#' myci(d, .95)

myci=function(d, confInt) {
  n = length(d)
  alpha = 1 - confInt
  mp = c(-1,1)
  conf = mean(d) + (mp*qt(1-alpha/2, n-1)*(sd(d)/sqrt(n)))

  return(conf)
}
