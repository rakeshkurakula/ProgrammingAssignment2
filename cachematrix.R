# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of matrix rather than computing it repeatedly. The
# following two functions are used to cache the invesre of a matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  #setter for the matrix
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  # getter for the matrix
  get <- function() x
  # setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  # getter for the inverse
  getinv <- function() inv
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the results and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}
<<<<<<< HEAD

## Sample run:
##> x = rbind(c(2, -2/4), c(-3/4, 3))
##> m = makeCacheMatrix(x)
##> m$get()
##      [,1] [,2]
##[1,]  2.00 -0.5
##[2,] -0.75  3.0

## No cache in the first run
## > cacheSolve(m)
##          [,1]       [,2]
##[1,] 0.5333333 0.08888889
##[2,] 0.1333333 0.35555556


## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##          [,1]       [,2]
##[1,] 0.5333333 0.08888889
##[2,] 0.1333333 0.35555556
## > 
=======
>>>>>>> origin/master
