## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  # list containing functions to
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
 
  inv = NULL
  set = function(y) {
     x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## To calculate the inverse of the matris based on the input made in Function makeCacheMatrix()
  
  inv = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
       message("getting cached data")
    return(inv)
  }
  
  # Calculating the inverse if it's not been Cached
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # To Cache after calculating the inverse
  x$setinverse(inv)
  
  return(inv)
}
