## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It contains four functions:
## get() - returns the current value of x
## set(y) - sets the value of x to the input parameter
## getinverse() - gets the current value stored in the inverse matrix
## setinverse(matrix) - sets the value to be stored in the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   #return the current value of inverse matrix (the cached matrix)
    m <- x$getinverse()
    
   # if the value is not null (has been calculated before), return it.  
   if(!is.null(m)) {
    message("getting cached data")
    return(m)
   }
  ## otherwise, calculate the inverse matrix, store it and return the newly 
  ## calculated value.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
