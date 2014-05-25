## This pair of functions cache the inverse of a matrix 

## This function creates a special "matrix" object with a list of functions
makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  #gets matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv #gets inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Computes the inverse of a matrix or retrieves the inverse
## from the caches

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
