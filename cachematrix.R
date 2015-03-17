## This function is called with (as its only argument) the matrix to be analyzed and returns a matrix modified to be a list of 4 functions, which are used to: 
## set/get the value of the matrix; 
## set/get the cached inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) { # changes the matrix and resets the cached inverse matrix
    x <<- y 
    inv <<- NULL 
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Asks for a matrix modified by makeCacheMatrix;
## checks if the inverse has already been calculated: if it was, it returns the cached result. 
## If it wasn't, it calculates the inverse, caches and returns it. 


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
