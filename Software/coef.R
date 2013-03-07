coef.report <- function(mod, name=NULL,intercept=FALSE){
  regress <- summary(mod)$coef
  df.model <- sum( !is.na(coef(mod))) # or use $rank ??
  m = nrow(regress)
  if( is.null(name)) name <- all.vars(mod$call)[1]
  res = data.frame(
    coef=regress[,1], se=regress[,2],t=regress[,3],p=regress[,4],
    df.model=rep(df.model,m),
    df.resid=rep(mod$df.residual,m),
    name=rep(name,m) )
  if( intercept ) return(res)
  else return(res[-1,])  # drop the intercept
}