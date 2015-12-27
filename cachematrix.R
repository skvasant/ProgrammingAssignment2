## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## Example from the function below:
## > x <- makeCacheMatrix(matrix(c(2, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE))
## > cacheSolve(x)
## [,1] [,2]
## [1,]  0.5    0
## [2,]  0.0    1
## > cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]  0.5    0
## [2,]  0.0    1
## > x <- makeCacheMatrix(matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE))
## > cacheSolve(x)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse
  setinv <- function(solve) m <<- solve
  ## get the value of the inverse
  getinv <- function() m
  ## the following line stores the four functions:
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() ## get the value of the inverse
  if(!is.null(m)) { ## check if the inverse is calculated already and matrix has not changed
    message("getting cached data")
    return(m) ## return the cached inverse value and exit the funtion
  }
  ## at this point it is determined the matrix has changed or the funtion is called the
  ## first time
  data <- x$get() ## get the value of the matrix
  m <- solve(data, ...) ## get the inverse of the matrix
  x$setinv(m) ## set the value of the inverse
  m
}
