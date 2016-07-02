## Put comments here that give an overall description of what your
## functions do

##  creates a special "matrix" object and caches its inverse
## it has four functions
#1) set- assign values to the matrix
#2) get- displayes values of the matrix
#3) setinverse - sets the inverse
#4) getinverse- outputs the inverse
makeCacheMatrix <- function(x = matrix()) {
  rev<- NULL
  
  # only square matrix are invertible
 +    dims <- dim(x)
 +    if (dims[1] != dims[2])
 +    {
 +        message("Matrix should be square")
 +    }
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  
  setrev <- function(solve) rev <<- solve
  getrev <- function() rev
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  rev <- x$getrev()
  if(!is.null(rev)) {
    message("getting cached data")
    return(rev)
  }
  data <- x$get()
  rev <- solve(data, ...)
  x$setrev(rev)
  rev
}
