## Put comments here that give an overall description of what your
## functions do

## This function creates the matrix that allows for caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Function to get the matrix
  get <- function() x
  
  #function to invert the matrix and set its value
  setSolve <- function(solve) m <<- solve
  
  #function to get the value of the matrix inverse
  getSolve <- function() m
  
  #return a list of the cached matrix
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## This function either returns the cached value of the inverse of a matrix
## or inverts the matrix and caches the value of the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  
  #Check to see if the inverse if cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If the inverse value is not cached, invert the matrix
  data <- x$get()
  m <- solve(data, ...)
  
  #Cache the value of the inverse
  x$setSolve(m)
}
