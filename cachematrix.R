## producing a square matrix using makeCacheMatrix will cahe the inversion
## of that matrix. it is assumed that the matrix is invertible.

## function storing the result using the <<- operator and the setter/getter functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list( set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve is used to return the inversion of x - given that x 
## was produced with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached <- x$getInverse()
  if(!is.null(cached)) {
    return(cached)
  }
  cached <- solve(x$get())
  x$setInverse(cached)
  cached
}
