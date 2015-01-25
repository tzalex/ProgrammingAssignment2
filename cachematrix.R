## Two functions that cache the inverse of a square
## invertible matrix, based on the Example: Caching
## the Mean of a Vector.
## Computing the inverse of a square matrix can be done
## with the solve function. If x is a square invertible
## matrix, then solve(x) returns its inverse.
## for testing:
## > a<-matrix(c(1,2,3,4),2,2)
## > b<-makeCacheMatrix(a)
## > cacheSolve(b)
## the result should be:
##     [,1] [,2]
## [1,] -2  1.5
## [2,]  1 -0.5


## The first function creates a special "matrix" object
## that can cache its inverse matrix, using the solve()
## function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse matrix has already been calculated
## (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
