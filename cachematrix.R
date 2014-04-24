# This goal of this function is to cache potentially time-consuming 
# computations. For example, taking the mean of a numeric vector is 
# typically a fast operation. However, for a very long vector, 
# it may take too long to compute the mean, especially if it has to be 
# computed repeatedly (e.g. in a loop). If the contents of a vector 
# are not changing, it may make sense to cache the value of the mean 
# so that when we need it again, it can be looked up in the cache rather 
# than recomputed.


## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the vector
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
  # get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of that special matrix
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
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
