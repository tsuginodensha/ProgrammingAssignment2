## Put comments here that give an overall description of what your
## functions do
## set(y) stores matrix, set cache inv to NULL
## get() gets matrix
## setinv(solve) calculates the inverse
## getinv() retrieves the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## checks if cached matrix is available.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv       ## Return a matrix that is the inverse of 'x'
}
