CODE:
## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#' Compute and cache the inverse of a matrix
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

[CacheMatrix.zip](https://github.com/jaztor/Quiz3-ProgrammingAssignment2/files/6777742/CacheMatrix.zip)
