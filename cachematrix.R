## Put comments here that give an overall description of what your
## functions do

## Create a matrix containing fields for the input matrix's inverse's cache
## Once the function has been called for the first time (given the input matrix) the inverse
## can be retrieved from this matrix instead of computing it a second time

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Returns the inverse of the input (cache) matrix
## If the inverse has not yet been computed, it will be calculated.
## Else the cached inverse will be retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
