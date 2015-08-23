## Elizabeth - cacheMatrixProject
## Creates what is initially an empty cache to hold a matrix's inverse so 
## that it does not need to be recalculated later if operation already performed

## Makes a cache to store a matrix x and its inverse - but doesn't go anything until a 
## a matrix x is supplied

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <-function(solve) m <<- solve
  getinverse <-function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of the matrix x if already calculated and stored
## otherwise calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached inverse")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

