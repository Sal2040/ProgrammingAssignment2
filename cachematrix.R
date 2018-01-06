## the two functions calculate an inverse matrix and store it into the global environment
## when called, the inverse matrix is retrieved from the global environment as long as the input 
## matrix is unchanged

## this function creates an object x, which is a square matrix to be inversed, and an empty object m
## into which an inverse matrix will be stored
## in addition, it creates a list of set/get functions called by the function cacheSolve
## all the objects are stored in the global environment so they can be called by other functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function takes the list created by the first function and calculates an inverse matrix 
## to matrix x (argument of the first function).
## it stores it into the object m in case m is empty. otherwise it only returns the already existing
## value of m
## updating an input matrix is possible through either running the "set" function from makeCacheMatrix,
## or by running the makeCacheMatrix with a new argument

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)     
}

  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}