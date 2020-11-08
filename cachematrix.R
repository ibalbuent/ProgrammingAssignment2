
## This code is useful to acces the inverse of a matrix that is used often, by
## storing it on the cache memory.


## Function makeCacheMatrix takes an object containing vectors of the same length
## as the object´s, i.e. a square matrix. This function creates a list 
## containing 4 functions which store the matrix and its inverse in cache.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list( set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## Function cacheSolve takes as argument the list returned by the first function
## and displays the inverse if already in cache or calculates it and displays it
## next.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting inverse from cache")
    return(inv)
  }
  dat <- x$get()
  inversemat <- solve(dat)
  x$setinv(inversemat)
  inversemat
}
