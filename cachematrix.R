## This function creates a special matrix object and sets inverse calculation in cache. 
## This is helpful as matrix inversion can be a costly computation and there may be some
## benefit to caching the inverse of a matrix instead of computing it repeatedly.
## Below are a pair of functions that are used to create a special object that stores a matrix
## (makeCacheMatrix) and caches its inverse (cacheSolve)

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                   ## assign inv as NULL value
  set <- function(y) {          ## in case user wants to resert matrix
    x <<- y                     ## reassigns new matrix to x
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes inverse of the matrix created by makeCacheMatrix. Given the matrix has 
## already been calculated and has not changed, then the cacheSolve function should retrieve
## the inverse from the cache. 

cacheSolve <- function(x, ...) {       
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()                ## to get calculated matrix
  inv <- solve(mat, ...)        ## calculates inverse matrix
  x$setInverse(inv)             ## reassign inverse matrix
  inv                           ## print inverse matrix
}
