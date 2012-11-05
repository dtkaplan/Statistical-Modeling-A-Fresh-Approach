getDJIAdata <- function() {
  djia <- fetchData("djia-2011.csv")
  djia <- transform(djia, Date=as.Date(Date))
  inds <- order(djia$Date)
  return(djia[inds,])
}
  