## A matrix of 3 row and 4 column is set, and the solved value,s is set as NULL


makeCacheMatrix <- function(x = matrix(c(1:12),3,4)) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## Inversing the square matrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inverse matrix")
    return(s)
  }
  data <- x$get()
  s <- mean(data, ...)
  x$setsolve(s)
  s
}
