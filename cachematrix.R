## Put comments here that give an overall description of what your
## functions do

## A "special matrix' is create by function makeCacheMatrix
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## After the function makeCacheMatrix create a "special matrix"
## cacheSolve is a function which solve the inverse of this matrix.

cacheSolve <- function(x, ...) {

  inverseMatrix <- x$getinverse()
  
  if(!is.null(inverseMatrix)) {
    message("Getting Cached Data.")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data,...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}