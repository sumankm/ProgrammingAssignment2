## cacheMatrix.R
# It is intended to write two functions: the first to make a matrix object with getters and setters;
# and the second function to write a function that inverts a matrix.
# As matrix operations are expensive, the second function should cache the inverted matrix.

## This is the function that makes a matrix object

makeCacheMatrix <- function(x = matrix()) {

  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	

}


## This function inverts the matrix and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("Fetching cached inverse matrix ...")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }

}

