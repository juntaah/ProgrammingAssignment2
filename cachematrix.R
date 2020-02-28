## There are two functions here: makeCacheMatrix() and cacheSolve().
## Together, these functions return the inverse of a matrix and stores it to cache.
## Any repeat calls of the function will retrieve the value from cache, saving us from re-computing it.

## In the first function below, we pass in a square matrix.
## The function stores the matrix values and defines functions that allows us to store and retreive again the inverse when the second function is called.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverted) inv <<- inverted
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function is the child function that does the work of inverting the matrix set using the first function.
## It calls functions from its parent environment to check for whether the inverse has already been calculated and set.
## If it has, the function returns it from cache and logs a message to that effect.
## If it hasn't, it calculates the inverse and sets it to the 'inv' symbol in the parent environment, available to access in future.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv  
}

## Try this for example by running the following:
## testmatrix <- matrix(1:4,2,2)
## cachedMatrix <- makeCacheMatrix(testmatrix)
## cacheSolve(cachedMatrix)
