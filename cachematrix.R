## Caches the Inverse of a Matrix
## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   # inverse - reset to NULL each time makeCacheMatrix
  # is called.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #defined by not run when makeCacheMatrix
  # is run.  
  
  get <- function() {x}    #returns the value of the 
  # original vector                 
  
  setinverse <- function(inverse) {m <<- inverse}  
  #stores value using superassignment <<-
  getinverse <- function() {m}  #returns cached value if avail. 
  
  #a list of the internal functions(methods), so a calling function
  #knows how to access those methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
