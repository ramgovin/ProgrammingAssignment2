## Put comments here that give an overall description of what your
## functions do

# This makeCacheMatrix function works like a class
# It creates a list that contains 4 member functions: set, get, setInv and getInv.
# It uses <<- assignment operator so that these internal variables are not exposed to the
# outside environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  # Variable that stores Inverse Matrix for x
  InvMx <- NULL  
  # Setter function to set matrix x
  set <- function(y) {
    x <<- y
    InvMx <<- NULL
  }
  # Getter function to get matrix x 
  get <- function() x
  # Calculate Inverse Matrix by using Solve function and set to InvMx 
  setinv <- function(solve) InvMx <<- solve
  # Returns Inverse Matrix for x 
  getinv <- function() InvMx
  
  #Return a list that contains functions to set, get, setinv, getinv operations on matrix x, InvMx
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)

}


# This cacheSolve function looks for valid Inverse Matrix for x in the cache.
# If cache variable returns (InvMx) not null, then returns cached Inverse Matrix for x
# otherwise, calculates Inverse Matrix for x and stores in the InvMx to use for 
# further calls without recalculating from Matrix x 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMx <- x$getinv()
  ## If InvMx is not null in the cache, then return Inverse Matrix for x from cache 
  if(!is.null(InvMx)) {
    message("getting cached data")
    return(InvMx)
  }
  ## If InvMx is null, then calculate Inverse Matrix for x
  data <- x$get()
  InvMx <- solve(data, ...)
  ## Since Inverse Matrix of x is not valid or null in cache, set InvMx to calculated Inverse Matrix for x
  x$setinv(InvMx)
  ## Return calculated Inverse Matrix for x 
  InvMx
}


# Test Cases
# Generate a random square, non-singular matrix using runif
# t <- matrix(runif(9,1,500),3,3)
# Generate the makeCacheMatrix object with this matrix
# tCached <- makeCacheMatrix(t)
# Going forward, retrieve calculated Inverse Matrix using the cacheSolve function
# cacheSolve(tCached)
# cacheSolve(tCached)
# cacheSolve(tCached)

