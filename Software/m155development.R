listFun <- function(...) {
  dots <- list(...,..=NA)
  nm <- names(dots)
  f <- function(x) {
    y <- x <- as.character(x)
    y[!(x %in% nm)] <- ".."
    r <- unlist(dots[y])
    names(r) <- x
    return(r)
    }
    return(f)
}
# Set the handling of NAs
options( na.rm=TRUE)