cost <- function(H,y,theta) {
  result <- crossprod(H - y)/(2*length(y))
}