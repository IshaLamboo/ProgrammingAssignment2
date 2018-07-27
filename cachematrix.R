## Calculate the inverse of a matrix (with caching)
## 
## Usage: 
##   1. Pass an invertible matrix to makeCacheMatrix and store the resulting object
##      This object has the input matrix, (after calling cacheSolve) cached matrix 
##      and functions to get and set those values.
##
##   2. Call cacheSolve() with the new object to get the (cached) inverse. Repeated
##      calls will return the cached value, use the set() function from the object
##      to initialize it with a new matrix.


## The makeCacheMatrix function creates a custom object with original and inverse matrix
## as well as a list of functions to set and get the matrix and its cached inverse.
## Each of the functions accesses the variables from the main object.

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function(neworiginal) {
    x <<- neworiginal
    cachedinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) cachedinverse <<- newinverse
  getinverse <- function() cachedinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes as input an object created by makeCacheMatrix, checks
## if the cachedinverse exists and either returns that or calculates the inverse anew.

cacheSolve <- function(x) {
  myinverse <- x$getinverse()
  
  if(!is.null(myinverse)) return(myinverse)
  data <- x$get()
  myinverse <- solve(data)
  x$setinverse(myinverse)
  myinverse
}
