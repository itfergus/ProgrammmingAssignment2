## Functions to calcuate and cache the inverse of a matrix (memoization)

## makeCacheMatrix has 4 functions:
# 1. Set value of matrix--> set()
# 2. Get value of matrix--> get()
# 3. Set function to solve inverse --> setinverse()
# 4. Get value of inverse --> getinverse()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y, n) {  # Must be square matrix to have inverse, so only one dimensional parameter
    dim(y) <- c(n, n)
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  # Solve() --> finds inverse of square matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates inverse of matrix from makeCacheMatrix function 
# If parameters already passed, returns inverse from cache to optimize function call
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
