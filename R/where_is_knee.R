where_is_knee <- function(points = NULL) {

  lower.limit <- 2
  upper.limit <- length(points) -1

  second.derivative <- sapply(lower.limit:upper.limit, function(i) { points[i+1] + points[i-1] - (2 * points[i]) })

  w.max <- which.max(second.derivative)
  best.k <- w.max +1

  return(best.k)

}

#where_is_knee( c(1:10,100:150))
