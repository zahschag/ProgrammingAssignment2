## Put comments here that give an overall description of what your
## functions do

## This function will create a special "matrix" obj that will cache the invers "matrix'

makeCacheMatrix <- function(x = matrix()) {
  ivm <- NULL
  set <- function(y) {
    x <<- y
    ivm <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solvem) ivm <<-solvem
  getinversematrix <- function() ivm
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function will compute the inverse of the special "matrix" that is returened from the makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ivm <- x$getinversematrix()
  if(!is.null(ivm)) {
    message("getting inverse matrix")
    return(ivm)
  }
  data <- x$get()
  ivm <- solve(data)
  x$setinversematrix(ivm)
  ivm
}