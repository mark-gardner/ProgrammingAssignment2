## The following code contains two functions that can be used to
## first create a matrix and then return the inverse of that matrix.
## A cached variable is used to store the inverse of a matrix once
## it's been calculated such that it isn't required to be re-calculated
## each time the inverse function is called.

## Further detail on each of the functions below.


## The makeCacheMatrix function takes a matrix argument and using an internal 'set' function
## assigns this to a cached matrix variable, also creating a NULL cache variable to 
## hold the inverse of the matrix once created.
## It also contains - a 'get' function, which returns the cache matrix variable.
##   - a setinverse function, which assigns the inverse matrix to the cache inverse matrix variable.
##   - a getinverse function, which returns the cache inverse matrix variable.
## The function returns a list of the functions - set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function uses the R 'solve' function to
## determine the inverse of the matrix created using makeCacheMatrix.
## It first checks whether this has already been created and cached
## using the makeCacheMatrix getinverse() function.
## If so it returns the cache inverse matrix.
## If not is uses the R 'solve' function to determine the inverse
## both returning this and using the makeCacheMatrix setinverse() function
## to save it to the cache inverse variable.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

