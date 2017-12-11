## Put comments here that give an overall description of what your
## functions do

## Vector versions of solutions
makeVector <- function(x = numeric()) {
  
  ## initialize the mean variable
  m <- NULL
  
  ## function to put passed vector contents via y into the global x variable and clear the mean
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## function just returns the vector
  get <- function() x
  
  ## puts the mean into the m variable within makeVector environment
  setmean <- function(mean) {
    m <<- mean
  }
  
  ## function returns the stored mean via m variable
  getmean <- function() m
  
  ## returns all of the functions
  list(set = set, get = get, setmean = setmean, getmean = getmean)

}

cachemean <- function(x, ...) {
  
  ## puts the returning value of mean if available
  m <- x$getmean()
  
  ## checks if available and returns value if it is
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if mean is not available the run the get() function to get the vector data
  data <- x$get()
  
  ## put the mean of the vector data into m variable
  m <- mean(data, ...)
  
  # runs the setmean() function to set the mean of the vector in the makeVector object
  x$setmean(m)
  m
}

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
  
  setinv <- function(inv) {
    i <<- inv
  }
  
  getinv <- function() i
  
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
