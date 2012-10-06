# Make a function based on a list of discrete values

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

# Calculate the angle between two vectors or, more generally,
# between a vector and a subspace defined by a model
# Remember to suppress the intercept
angle <- function( object, v2, data=NULL, degrees=TRUE ) UseMethod("angle")
# =============
angle.default <- function(object, ... ) {
  stop("First argument should be a model, a formula, or a numerical vector.")
}
# ============
angle.numeric <- function(object, v2, data=NULL, degrees=TRUE){
  ang <- acos(sum(object*v2)/sqrt(sum(object^2)*sum(v2^2)))
  if (degrees) ang <- 180*ang/pi
  return(ang)
}
# =============
angle.logical <- function(object, v2, data=NULL, degrees=TRUE ) {
  angle.numeric( as.numeric(object), as.numeric(v2), degrees=degrees)
}
# =============
angle.formula <- function( object, v2, data=NULL, degrees=TRUE) {
  mod <- lm(object, data=data)
  angle.lm(mod,degrees=degrees)
}
# =============
angle.lm <- function(object, v2, data=NULL, degrees=TRUE) {
  # response is the first component of the "model" component
  response <- object$model[,1]
  angle.numeric(response,fitted(object), degrees=degrees)
}
