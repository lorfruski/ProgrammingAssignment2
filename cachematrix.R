## Matrix inversion is a potentially time-consuming operation.
## This file contains two functions that compute the inverse
## of a matrix, cache the inverse if it has already been calculated,
## and use the cached value if available.

## makeCacheMatrix takes as its input a matrix and returns a 
## list of functions that get and set the value of the matrix 
## and get and set the value of the matrix inverse.  The list of
## functions returned is "get", "set", "getinv", and "setinv".
## The matrix and its inverse are assigned in the environment of 
## these four functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## When setting a new value of the matrix, check first
    ## to see if the new value is the same as the old value (whether or
    ## not the variable name has changed).  If they are the same, do nothing.
    if(identical(x,y)) return()
    ## Cache the new value of the matrix in the environment of the 
    ## set, get, setinv, and getinv functions.
    x <<- y
    ## Set the cached value of the inverse to NULL because it hasn't been
    ## computed yet
    inv <<- NULL
  }
  ## Retrieve the cached value of the matrix 
  get <- function() x
  ## Cache the new value of the matrix inverse in the environment of
  ## the set, get, setinv, and getinv functions.
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve takes the list of functions returned
## by makeCacheMatrix and returns the matrix inverse,
## either by computing it or by retrieving its value
## from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## Check if the inverse has been previously calculated, and if so
  ## return the previous value for the inverse
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  ## Retrieve the value of the matrix so we can calculate the inverse
  mydata <- x$get()
  ## Calculate the inverse
  inv <- solve(mydata, ...)
  ## Cache the value for the inverse
  x$setinv(inv)
  ## Return the value of the inverse to the user
  inv
}

## Testing Statements
# solve(matrix(c(1,2,3,4), nrow = 2))
# 
#  test_m <- c(1:4)
#  dim(test_m) <- c(2,2)
#  test_m
# solve(test_m)
# 
# 
# thlup <- test_m
# identical(thlup, test_m )
# slorp$set(thlup)
# 
# slorp <- makeCacheMatrix(test_m)
# cacheSolve(slorp)
# undebug(cacheSolve)
# 
# slorp$set(test_m)
# 
# mymat <- matrix(runif(16) + 4, nrow = 4)
# mymat[1] <- -17
# mymat[2]   <- 6
# 
# mymat
# solve(mymat)
# slorp <- makeCacheMatrix(mymat)
# 
# 
# 
# x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2))
# x$get(x)
# 
# messfun <- function(x = 5,a = 4){
#   mymean <- function(x) a*mean(x)
#   mymean
# }
# 
# x$get <- messfun(x)
# x$getinv()
# 
# x$set(test_m)
# cacheSolve(x)
