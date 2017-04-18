## Functions for compulting and caching the inverse of matrix
## 'makeCacheMatrix' should be called before 'cacheSolve'
## If input matrix was changed, 'makeCacheMatrix' should be called again to update cache
## 
## e.g.
## 
## c <- makeCacheMatrix(x)
## cachesolve(c)
## 


## Creates a special vector for caching value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes inverse of matrix 'x', if it was computed before - returns previously saved inverse matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
