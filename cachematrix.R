## In the beginning inv is a 1 by 1 matrix that contains only one value (NA)
## It will be rewritten by the inverse matrix of x
## It will be overwritten by the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
  inv <- matrix() ## initiate inv with an 1 by 1 matrix with inv[1,1]=NA
  set <- function(y) { ## defines a function and save it in set
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  get <- function() x ## function that returns x
  setinv <- function(solve) inv <<- solve ## function that calculates the inverse and saves it
  getinv <- function() inv ## function that returns inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Since we assume that the matrix x is invertible, the inverse can not contain NAs.
## I used that as a test to check if the inverse was already stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.na(inv[1,1])) {
  inv <- x$getinv() ## gets the inverse
  if(!is.na(inv[1,1])) { ## checks if the first entry is NA
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  data <- x$get() ## gets matrix x
  inv <- solve(data) ## calculates the inverse of x
  x$setinv(inv) ## saves the inverse
  inv
}
