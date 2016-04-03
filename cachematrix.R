## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ##initialising inverse matrix
  inversemat <- NULL
  
  ##function for setting value of matrix
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  
  ##function for getting value of matrix
  get <- function() x
  
  ##function for setting value of inverse matrix
  setinverse <- function(imat) inversemat <<- imat
  
  ##function for getting value of inverse matrix
  getinverse <- function() inversemat
  
  ##return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##get value of inverse matrix from cache
  inversemat <- x$getinverse()
  
  ##check if cache value is not null and use the cached data and return
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  
  ##this part is executed if cached data was null
  ##fetch the data or the orginal matrix
  data <- x$get()
  
  ##calculate the inverse of the matrix
  inversemat <- solve(data)
  
  ##set the inverted matrix value in the cache
  x$setinverse(inversemat)
  
  ##return inverse matrix
  inversemat
}
