##The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
##First - set the value of the matrix
##Second - get the value of the matrix
##Third - set the value of the inverse of the matrix
##Fourth - get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## It first checks to see if the inverse has already been calculated. 
## If so, it retains the results the cache and skips the computation. 
## Otherwise, it calculates the inverse of the cache and sets the value of th the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
