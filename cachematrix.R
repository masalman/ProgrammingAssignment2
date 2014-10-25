## Mohamad Salman - 10/25/14
## R Programming - Assignment 2 - Lexical Scoping
## ***************************************************************
## The following are functions which illustrate lexical scoping
## written to satisfy rprog-008 Assignment 2 requirements.
## ***************************************************************
## Function: makeCacheMatrix
## Input: x (matrix)
## Sub-functions:
##  - set: receives input (y) in matrix form, stores in cache
##         variable x
##  - get: returns contents of cache variable x
##  - setinverse: sets the cache inverse value for the cache
##                matrix
##  - getinverse: returns the cache inverse value for the cache
##                matrix
## Description:
## Class Container which initializes, sets and retrieves the
## cached matrix and its respective inverse
##
## Cached objects:
## x - user-defined matrix
## m - the inverse of user-defined matrix
## ***************************************************************
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse
  m <- NULL
  # subfunction set, stores new user-defined matrix into cache
  # location x
  set <- function(y) {
      # store into cache location, clear the inverse
      x <<- y
      m <<- NULL
  }
  # subfunction get, retrieves user-defined matrix
  get <- function() x
  # subfunction setinverse, stores user-defined matrix inverse 
  # into cache location m
  setinverse <- function(inverse) m <<- inverse
  # subfunction getinverse, retrieves cached inverse m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## ***************************************************************
## Function: makeCacheMatrix
## Input: x (matrix)
## Sub-functions:
##  - set: receives input (y) in matrix form, stores in cache
##         variable x
##  - get: returns contents of cache variable x
##  - setinverse: sets the cache inverse value for the cache
##                matrix
##  - getinverse: returns the cache inverse value for the cache
##                matrix
## Description:
## Class Container which initializes, sets and retrieves the
## cached matrix and its respective inverse
##
## Cached objects:
## x - user-defined matrix
## m - the inverse of user-defined matrix
## ***************************************************************
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## retrieve the cached inverse
        m <- x$getinverse()
        # if the inverse has been calculated, return cached inverse
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        # otherwise, calculate inverse. Fetch the inverse
        data <- x$get()
        # use R 'solve' function to calculare inverse
        # for more details, type '?solve' in console
        m <- solve(data, ...)
        # store inverse
        x$setinverse(m)
        m
}
