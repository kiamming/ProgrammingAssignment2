## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newInv) inv <<- newInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}

source("cachematrix.R")

# Creating a cached matrix
testMatrix = makeCacheMatrix(matrix(1:4, 2, 2))

# Printing the cached matrix
testMatrix$get()

# Printing the inverse, if any
testMatrix$getinv()

# Solving for the inverse and caching it 
cacheSolve(testMatrix)

# Calling cacheSolve again. This time, it gets the cached data
cacheSolve(testMatrix)
