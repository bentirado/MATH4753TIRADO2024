#' The myncurve Function
#'
#' @param mu the mean of the data
#' @param sigma the standard deviation of the data
#' @param a the upper bound of the area calculation
#' @param listnum the number 1<=listnum<=3 that selects the output from the linked list
#'
#' @return a graphical representation of the curve along with a named list output.
#' @export
#'
#' @examples
#' myncurve(10, 3, 1, 2)


myncurve = function(mu, sigma, a, listnum=1){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(0,a, length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(0,xcurve,a),c(0,ycurve,0),col="Red")

  area=pnorm(a,mean=mu,sd=sigma)-pnorm(0,mean=mu,sd=sigma)
  area=round(area,4)

  list(mu = mu, sigma = sigma, area=area)


}
