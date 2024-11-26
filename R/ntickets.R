#' The ntickets Function
#'
#' @param N an integer stating the amount of "seats" in the flight
#' @param gamma an integer between 0 <= gamma <= 1 which is the probability that the plane will be overbooked
#' @param p an integer between 0<= p <= 1 which is the probability that a seat "shows"
#'
#' @return two graphs discrete and continuous, which display the n value of the distribution.
#' @export
#'
#' @examples
#' ntickets(N=200, gamma=0.02, p=0.95)

ntickets = function(N=200, gamma=0.02, p=0.95){

  #Create n vector.
  n = seq(N, floor(N+N/10), by = 1)
  #Creating discrete distribution.
  tmp = 1 - gamma - pbinom(q=N, size=n, prob=p)
  ind = which.min(abs(tmp))
  #N-Discrete
  nd = n[ind]

  #Creating continuous distribution.
  continuous <- function(n) {
    1 - gamma - pnorm(N + 0.5, n * p, sqrt(n*p*(1-p)))
  }

  #Finding the n using uniroot.
  nc <- uniroot(continuous, interval = c(N, floor(N+N/10)))$root

  #Making discrete plot
  plot(n, tmp, col="Blue", pch=19, cex=.8, main=paste0("Objective Vs n to find optimal tickets sold\n (",nd, ") gamma= ", gamma, " N=", N, " discrete"), ylab="Objective")
  abline(v=nd, h = 0, col="Magenta", lwd= 2.5)

  #Making continuous plot
  n2 = seq(N, floor(N+N/10), by = 0.05)
  tmp2 = 1 - gamma - pnorm(N + 0.5, n2 * p, sqrt(n2*p*(1-p)))
  plot(n2, tmp2, col="Black", type = "b", pch=20, cex=.50, main=paste0("Objective Vs n to find optimal tickets sold\n (",nc, ") gamma= ", gamma, " N=", N, " continuous"), ylab="Objective", xlab="n")
  abline(v=nc, h = 0, col="Green", lwd= 2.5)

  #List for output.
  list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)

}
