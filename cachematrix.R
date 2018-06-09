## Assignment: Caching the Inverse of a Matrix
## This code represents a pair of functions that cache the inverse of a matrix


## this function creates a special "matrix" object that can cache its inverse (solve function)

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setInverse = function(solveMatrix) inv <<- solveMatrix
  getInverse = function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
        ## Return a matrix that is the inverse of 'x'
}
