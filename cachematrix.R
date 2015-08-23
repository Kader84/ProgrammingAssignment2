## For test :
## x <- matrix(rnorm(25), nrow = 5) 
## y <- makeCacheMatrix(x)
## y$get()
## cacheSolve(y)
## cacheSolve(y)  // For 2nd time, return the cached inverse

## makeCacheMatrix : return a list of functions to :
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse
## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Store the value of the inverse
  inv <- NULL
  
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse
  s_inverse <- function (inverse) inv <<- inverse
  
  #Get the value of the inverse
  g_inverse <- function () inv
  
  #list of the previous fonctions
  list(set = set , get = get, 
       s_inverse = s_inverse, 
       g_inverse = g_inverse)
}


## cacheSolve : compute the inverse of the matrix. If the inverse is already exist
##              it returns the cached inverse

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  inv <- x$g_inverse()
  
  # if exist, getting cached data
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  # The inverse not exist, so we calcultes it
  data <- x$get()
  inv <- solve(data,...)
  
  #cache the inverse
  x$s_inverse(inv)
  
  #Return the inverse
  inv
}
