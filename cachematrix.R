## This is programming assignment 2 for R programming
## There are two functions, makeCacheMatrix and cacheSolve
## cacheSolve checks if there is an inverted matrix in the cache, if not then solve and cache
## makeCacheMatrix provides the tools for the operation and storing values in appropriate environments


## makeCacheMatrix makes a matrix into a list of four functions 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## set the matrix to y
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
## return the matrix
  getMatrix <- function() x
## set the cache solution to inverted
  setInver <- function(inverted) m <<- inverted
## return the cached solution
getInver <- function() m
## makeCacheMatrix returns the list of the four functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInver = setInver,
       getInver = getInver)
}


## cacheSolve checks if there is a cached solution (), if not then calculate and caches one, returns inverted matrix

cacheSolve <- function(x) {
## Is there an inverted solution, if there is, print a message and the solution 
  m <- x$getInver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
## if there was no solution, get the original, invert it, cache and return the solution
  data <- x$getMatrix()
  m <- solve(data)
  x$setInver(m)
  m
}
