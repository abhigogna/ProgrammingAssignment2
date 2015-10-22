## This	functions takes a square matrix eg: (2X2) or (3X3) etc.
## and caches matrix inverse. It returns gets and sets a matrix
## and its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getinverse = getinverse)

}


## This function calculates matrix inverse if the 
## makeCacheMatrix does not have the inverse stored
## in cache. The matrix inverse is then set in
## makeCacheMatrix, so, subsequent calls to calculate
## to matrix inverse returns the cached value as 
## long as matrix has not changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m 
}
