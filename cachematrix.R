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
##
## Description:
## Class Container which creates a cache matrix. Initializes, 
## sets and retrieves the cached matrix and its respective inverse
##
## Cached objects:
## x - user-defined matrix
## inv_cache - the inverse of user-defined matrix
## ***************************************************************
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse
  inv_cache <- NULL
  # subfunction set, stores new user-defined matrix into cache
  # location x
  set <- function(y) {
      # store new matrix into cache location, clear the inverse
      x <<- y
      inv_cache <<- NULL
  }
  # subfunction get, retrieves user-defined matrix x
  get <- function() x
  # subfunction setinverse, stores user-defined matrix inverse 
  # into cache location inv_cache
  setinverse <- function(inverse) inv_cache <<- inverse
  # subfunction getinverse, retrieves cached inverse inv_cache
  getinverse <- function() inv_cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## ***************************************************************
## Function: cacheSolve
## Input: x (matrix)
## Output: m (matrix), inverse of x
##
## Description:
## Matrix inverse solver. Takes a user-defined matrix 'x' and will:
##     (a) calculate the matrix inverse  using R function 'solve'
##         if not defined, storing in cache location
##     (b) return a cached matrix inverse if previously calculated
##    
## Notes:
## Not all matrices have inverses. The user-defined input matrix
## should be such that its determinant != 0.
## 
## Additional reading:
## http://en.wikipedia.org/wiki/Invertible_matrix
##
## Sister functions: makeCacheMatrix()
## ***************************************************************
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## retrieve the cached inverse
        inv <- x$getinverse()
        # if the inverse has been calculated, return cached inverse
        if(!is.null(inv)) {
              message("getting cached data")
              # return inverse
              return(inv)
        }
        # otherwise, calculate inverse. Fetch the inverse
        data <- x$get()
        # use R 'solve' function to calculare inverse
        # for more details, type '?solve' in console
        inv <- solve(data, ...)
        # store inverse
        x$setinverse(inv)
        # return inverse
        return(inv)
}
## ***************************************************************
## Sample outputs:
## Links to Wolfram Alpha per Matrix to validate solution
##
## > m <- makeCacheMatrix()
##
## Matrix Example 1:
## Wolfram Alpha Link: http://goo.gl/T2s74D
##
## > m$set(matrix(c(1,0,0,1),2,2))
## > m$get()
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(m)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## Matrix Example 2:
## Wolfram Alpha link: http://goo.gl/XMgwRX
##
## > m$set(matrix(c(2,0,2,0),2,2))
## > m$get()
## [,1] [,2]
## [1,]    2    2
## [2,]    0    0
## > cacheSolve(m)
## Show Traceback
## 
## Rerun with Debug
## Error in solve.default(data, ...) : 
## Lapack routine dgesv: system is exactly singular: U[2,2] = 0 
##
## Matrix Example 3:
## Wolfram Alpha link: http://goo.gl/lhvXci
##
## > m$set(matrix(c(2,2,0,0),2,2))
## > m$get()
## [,1] [,2]
## [1,]    2    0
## [2,]    2    0
## > cacheSolve(m)
## Show Traceback
## 
## Rerun with Debug
## Error in solve.default(data, ...) : 
## Lapack routine dgesv: system is exactly singular: U[2,2] = 0 
## 
## Matrix Example 4:
## Wolfram Alpha Link: http://goo.gl/85Nh2Z 
##
## > m$set(matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3))
## > m$get()
## [,1] [,2] [,3]
## [1,]    7    2    1
## [2,]    0    3   -1
## [3,]   -3    4   -2
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## > cacheSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## ***************************************************************
