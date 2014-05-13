# Matrix inversion with caching

# How to use!
# m <- makeCacheMatrix(x = matrix())
# m$get()     # will display the matrix
# m$set(y)   # will (re)set the matrix to be the matrix y
# i <- cacheSolve(m)    # will invert the matrix m 
# j <- cacheSolve(m)    # will retrieve the cached inverse


## makeCacheMatrix creates a special "matrix" which is really 
## a list containing functions to:
## 1. set the value of the matrix object
## 2. get the value of the matrix object
## 3. set the value of a matrix
## 4. get the value of the matrix inverse
##


makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) {i <<- solve }
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve calculates the inverse of a special "matrix" created 
## with makeCacheMatrix.
## However, it first checks to see if the inverse has already
## been calculated.  If so, it gets the inverse from the cache
## and skips the computation.  Otherwise, it calculates
## the inverse and sets the inverse in the cache via
## the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if (!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     m <- x$get()
     i <- solve(m, ...)
     x$setinverse(i)
     i
}
