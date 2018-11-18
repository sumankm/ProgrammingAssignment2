## cacheMatrix.R
# It is intended to write two functions: the first to make a matrix object with getters and setters;
# and the second function to write a function that inverts a matrix.
# As matrix operations are expensive, the second function should cache the inverted matrix.

## The exercise requires an understanding of lexical scoping and simple matrices

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





## Here is how it works
## --------------------

# First we define an invertible matrix in m0
# --------------------------------------------
# > m0 <- matrix(c(4,2,7,6), nrow=2, ncol=2)
# > m0
#      [,1] [,2]
# [1,]    4    7
# [2,]    2    6



# Next, we initialize a matrix object
# -----------------------------------
# > m1 <- makeCacheMatrix(m0)


# Now we can calculate the inverse
# --------------------------------
# > cacheSolve(m1)
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4


# If we calculate the inverse the second time,
# it fetches the result from the cache!!
# --------------------------------------------
# > cacheSolve(m1)
# Fetching cached inverse matrix ...
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# >
