#Jacob Goren
#Assignment 2 for R Programming on Coursera
#5-23-2014

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix is a function to return a list of functions that:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initialize inv variable
  
  set <- function(y) { # set function for matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get function for matrix
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse # set function for matrix
  getinv <- function() inv #get function for the matrix
  # Return the functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the matrix hasnt changed and the inverse has already been calculated
# then the this function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv() #get the inverse
  
  #check if we already have it, if so return and move on
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # We didnt return it, so go calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the new variable
  x$setinv(inv)
  # Return the inverse
  inv
}
