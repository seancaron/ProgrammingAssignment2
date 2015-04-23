## These functions create a list of functions that accept a matrix, and produce the inverse of that matrix
## If the inverse has already been calculated, the cacheSolve function will pull the inverse from cache, rather
## than re-calculating the inverse.




## Build the list of functions for manipulating the matix. The matrix and its inverse are stored in the
## Global Environment, rather than the makeCacheMatrix environment, so that once produced they can be used
## again and again without recalculation
##
## functions in the list include set() and get() for the original matrix, and
## setinverse() and getinverse() for the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL                          ## init the inverse
      set <- function(y) {                     ## implement $set using <<- to populate the Global Env
            x <<- y
            inverse <<- NULL
      }
      get <- function() x                      
      setinverse <- function(solved) inverse <<- solved
      getinverse <- function() inverse
      list(set = set, get = get,               ## create and return the list of functions
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve checks to see if the matrix inverse already exists in the Global Environment (ie. is cached)
## using the $getinvers() function implemented above.
##
## if so, it simply returns the cached version without calculation
## if not, it calculates the inverse and stores it to the Global Environment via the $setinverse() function.
## 
## note that the cacheSolve() function does not need to know anything about the Global Environment cache itself;
## all of that functionality is handled by the makeCacheMatrix() and its implemented functions.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {                        ## m will be !NULL if Global Env doesnt have the inverse
            message("getting cached data")
            return(m)
      }
      data <- x$get()                          ## otherwise M is NULL - go ahead and solve()
      m <- solve(data, ...)
      x$setinverse(m)                          ## and post the result with $setinvers
      m
}
