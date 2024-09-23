#' The mybin function. Creates a binomial simulation with replacement.
#'
#' @param iter an integer number of iterations for each graph
#' @param n an integer number of the amount of successes that will be tested.
#' @param p an integer value from 0 <= p <= 1 that defines the probability of success with each trial.
#'
#' @return Binomial graphs created with the constraints provided.
#' @export
#'
#' @examples
#' mybin(iter=100, n=10, p=0.5)

mybin=function(iter=100,n=10, p=0.5){

  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
