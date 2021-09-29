## These functions allow us to cache the inverse of a matrix

## To create a special matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
p <- NULL
  set <- function(y){
  x <<- y
  p <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) p <<- inverse
  getInverse <- function() p 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## To compute the inverse of the special matrix returned by the previous function

cacheSolve <- function(x, ...) {
        ## A matrix that is the inverse of 'x' will be returned
p <- x$getInverse()
  if(!is.null(p)){
  message("getting cached data")
  return(p)
  }
  mat <- x$get()
  p <- solve(mat,...)
  x$setInverse(p)
  p
}
