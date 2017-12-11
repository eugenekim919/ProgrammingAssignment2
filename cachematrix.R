# Week 3 Assignment
# 12/11/2017
# Eugene Kim

###########################################################################
## Functions that create the cache matrix and to compute/save the inverse
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  # holder for the inversed matrix
  i <- NULL
  
  # create the set function that will set the matrix to the new value and clear the inverse value
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  
  # gets the contents of the matrix
  get <- function() x 
  
  # sets the inverse matrix variable at the makeCacheMatrix level
  setinv <- function(inv) {
    i <<- inv
  }
  
  # returns the inversed matrix
  getinv <- function() i
  
  # returns the functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a makeCacheMatrix object and returns/caches the inverse of the matrix passed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## puts the returning value of inverse if available
  i <- x$getinv()
  
  ## checks if inverse is available and returns value if it is
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if mean is not available the run the get() function to get the vector data
  data <- x$get()
  
  ## put the inverse of the matrix data into i variable
  i <- solve(data)
  
  # runs the setinv() function to set the mean of the vector in the makeVector object
  x$setinv(i)
  i
}
