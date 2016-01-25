## Put comments here that give an overall description of what your
## functions do

# This code will contain two functions.
# The first one creates a
# list to hold a matrix, its respective inverse.
# The list contains getter and setter functions for each value. 
#
# The second function takes the list created by the first function as an
# argument and returns the Inverse of the matrix.
# This setup allows the value of the inverse to be chached so that it will only be 
# evaluated once.

## Write a short comment describing this function
# makeCacheMatrix returns a list which can hold a 
# matrix and its inverse.
# getter and setter functions are used to access and set the respective values.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # getter and setter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # getter and setter for matrix inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  # return a list of the functions defined above.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function takes a list like that returned by the makeCacheMatrix function
# and returns the inverse of the matrix.
# Caching allows for the inverse coputation to be performed only the first time the function
# is called for a specific list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # if the inverse has already been calculated and assigned, return the value and exit.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Computation of inverse below only occurs if the inverse has not already been calculated.
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
