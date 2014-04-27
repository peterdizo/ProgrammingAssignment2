## Functions needed for the R programming assignment 
## written by Peter Dizo

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # clear s
  s <- NULL
  
  # create the set method to set new matrix and clear the cache 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # create the get methot to get the matrix
  get <- function() x
  
  # create the setinv method to set the calculated matrix inverse
  setinv <- function(inv) s <<- inv
  
  # create the getinv method to get the calculated matrix inverse
  getinv <- function() s
  
  # create a list of functions, so we can use them (as in the vector example)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # load the precalculated cache
  s <- x$getinv()
  
  # check if it is not empty
  if(!is.null(s)) {
    message("getting cached data")
    
    # return cached inverse, if there is any
    return(s)
  }
  
  # if there is no cached inverse, we have to calculate it
  # first, get the original matrix
  data <- x$get()
  
  # secondly, calculate the inverse using the solve() function
  s <- solve(data)
  
  # save the result to the cache
  x$setinv(s)
  
  # return the calculated result
  s
}
