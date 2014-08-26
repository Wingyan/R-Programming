# Wing Yan Fan
# 08/18/2014
# R Programming 
# Assignment 2


##-------------------------------------------------------------
## Description:
## Caching the Inverse of a Matrix
##
## Sample run:
## > mat <- matrix(c(4,2,7,6), nrow=2, ncol=2)
## > mat1 <- makeCacheMatrix(mat)
## > mat1$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > mat1$getmatrix()
## NULL
## > cacheSolve(mat1)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(mat1)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > mat1$getmatrix()
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##-------------------------------------------------------------

##-------------------------------------------------------------
## Creates a special "matrix" object that can cache its inverse.
##-------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## get and set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## get and set the solve value
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix= setmatrix,
       getmatrix = getmatrix)
}

##-------------------------------------------------------------
## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If it has been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##-------------------------------------------------------------
cacheSolve<- function(x, ...) {
  m <- x$getmatrix()
  
  ## get and return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## calculate the inverse matrix since it has not been cached
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}