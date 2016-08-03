##R Programming Assignment Week3 
## Function makeCacheMartix will used in cacheSolve 
## It will return the list of functions
## set the matrix,Get matrix, Set INverse and Get inverse

## this function will take martix as the input

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse , getinverse = getinverse)
}


## Function cacheSolve will use list generated from makeCacheMatrix 
## It will return inverse of the matrix
## this function will take martix as the input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  
  x$setinverse(i)
}
