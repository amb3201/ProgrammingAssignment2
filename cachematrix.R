## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix contains the set, get, set inverse 
## mean, and get inverse mean functions for the argument x. The 
## output is a list of results.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize im to hold the inverse matrix result
  im <- NULL
  
  ## Set function
  set <- function(y) {
    ## Set x equal to y and im to null in parent environment
    x <<- y
    im <<- NULL
  }
  
  ## Get function
  get <- function() x
  
  ## Set inverse matrix
  setinversematrix <- function(solve) im <<- solve
  
  ## Get inverse matrix
  getinversematrix <- function() im
  
  ## Name the results of the list
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Write a short comment describing this function
## The function cacheSolve is used to populate and retrieve the inverse
## matrix from the object makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinversematrix()
  
  ## Check to see if there is already a cached result, 
  ## if there is, return the cached IM value
  if(!is.null(im)) {
    message("getting chached data")
    return(im)
  }
  
  ## If im is null, calculate and populate the inverse matrix
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  im
}