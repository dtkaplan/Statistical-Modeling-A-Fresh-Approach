takeLag <- function( x, lag=1) {
  if (lag < 1 | round(lag)!=lag) stop("lag must be an integer")
  pre = x[1:(length(x)-lag)]
  post = x[(1+lag):length(x)]
  return( data.frame( pre=pre, post=post))
}