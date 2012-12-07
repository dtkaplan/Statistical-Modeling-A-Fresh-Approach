fixCheckbox <- function(nm, data, alone=FALSE,...){
  if( missing(data)) stop("Must provide data= data frame.")
  nm = as.character(substitute(nm)) # convert first argument to a name
  if( !nm %in% names(data)) stop("First argument must be a name in the data frame.")
  V <- as.character(data[[nm]])
  L <- levels( data[[nm]] )
  kill <- list("\\(.+?\\)"," ",...) # Strings to eliminate
  for (k in 1:length(kill)) {
    V <- gsub(kill[[k]],'',V)
    L <- gsub(kill[[k]],'',L)
  }
  vars <- unique(unlist(strsplit(L,",",fixed=TRUE)))
  res <- matrix(FALSE,nrow=length(V),ncol=length(vars))
  for (k in 1:length(V)) {
    res[k,] <- vars %in% unlist(strsplit(V[k],",",fixed=TRUE))
  }
  res <- as.data.frame(res)
  colnames(res) <- paste(nm,vars,sep="")
  if(alone) return(res)
  else {
    data[[nm]] <- NULL
    return( cbind(data,res))
  }
}
# ==================

LikertToQuant <- function(nm, data=parent.frame(), order, ...){
  if( missing(order)) stop("Must give a numerical value for each level.")
  nm = as.character(substitute(nm)) # convert first argument to a name
  if( length(order) != length(levels(data[[nm]])))
    stop("Must provide as many quantitative values as levels in the categorical")
  newvar = order[as.numeric(data[[nm]])]
  return(newvar)
}


# =============
niceTable <- function(){
require(mosaic)
require(xtable)
options(xtable.type="html")
}