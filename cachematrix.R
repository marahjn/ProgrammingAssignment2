## The first function makeCacheMatrix creates a "matrix" object that can cache its inverse.
## The second function cacheSolve provides the inverse of the "matrix" returned by makeCacheMatrix
## by retrieving the inverse from the cache.

## makeCacheMatrix function creates a special object "matrix". 
##Set the value of the matrix, get the value of the matrix,
##set the inverse matrix, get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL 
    set <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
	
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse 
  getinverse <- function() invMatrix
  list( set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


##cacheSolve function uses the output of the previous function as its input.
## If the inverse has been calculated, cachesolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

	invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
	
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setinverse(invMatrix)
   
     invMatrix
}
