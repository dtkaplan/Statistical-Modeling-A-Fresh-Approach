require(manipulate)
require(mosaic)
coin.history <- c()

test.coins <- function(coins,spec=0.05) {
  mylongest <- max(rle(coins)$lengths)
  trials <- do(100)*max(rle(rbinom(length(coins),prob=.5,size=1))$lengths)
  if( mylongest > max(result, data=trials)) 
    return("Phony.  You just clicked without much variation.")
  if( mylongest < qdata(spec, result, data=trials ) )
    res <- "Cheater!"
  else {
    if( mylongest < qdata(.1, result, data=trials))
      res <- "Hard to say ..."
    else
      res <- "Honest flipper."
  }
  return(res)
  
}

enter.coins <- function(n=10) {
  coin.history <<- c()
  display <- function(head, tail, back) {
    if(back) {
      if( length(history)>1 ) coin.history <<- coin.history[-length(history)]
    }
    else if( (head | tail) & length(coin.history) < n ) {
      this <- ifelse( head, "H", "T" )
      coin.history <<- c(coin.history, this)
    }
    
    plot( 0:1,0:1,type="n",xaxt="none",yaxt="none",xlab="",ylab="",bty="n")
    text(1,.5,paste(coin.history,collapse=""),pos=2)
    text(.5,.1, paste(length(coin.history), "flips", "out of", n))
  }
  manipulate(display(head,tail,back), 
             head=button("H"), tail=button("T"), back=button("mistake"))
  return( coin.history )
}