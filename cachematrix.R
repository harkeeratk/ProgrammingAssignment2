## Put comments here that give an overall description of what your
## functions do

##  creates a special "matrix" object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  rev<- NULL
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
