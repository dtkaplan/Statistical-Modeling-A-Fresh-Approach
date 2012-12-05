heart.rate = function(who=1:5,
                      posture=rep("sitting",5)) {
  posture <- rep(posture, length.out=length(who))
  if (!all( posture %in% c("standing","sitting","lying")))
    stop("Posture must be 'standing', 'sitting', or 'lying'")
  base <- response <- weight <- rep(0,length(who))
  sex <- rep("F",length(who))

  for (k in 1:length(base)) {
    set.seed(who[k])
    base[k] <- runif(1,min=50,max=75) + rnorm(1,sd=who%%10)
    response[k] <- runif(1,min=1,max=5)
    sex[k] <- c("F","M")[1+round(runif(1))]
    weight[k] <- rnorm(1,mean=70-10*(sex=="F"),sd=15-3*(sex=="F")) 
  }
  offset <- response*((posture=="standing") - .3*(posture=="lying"))
  
  
  data.frame(who=as.factor(paste("S",who,sep="")), 
                           posture=as.factor(posture),
                           hr=round(base+offset -.04*weight - runif(length(who))*3*(sex=="F")),
                           sex=as.factor(sex),
                           weight=round(weight))
             
}
  