## This pair of functions calculate and cache the inverse of a matrix.

## Creates a "special" matrix object which has the capacity to store its
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y) {
                x <<- y
                x.inv <<- NULL
        }
        get <- function() x
        setinverse <- function(z) x.inv <<- z
        getinverse <- function() x.inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a given matrix that is stored in the special object
## makeCacheMatrix. If the inverse has already been calculated by this function
## then the inverse matrix will be cached within the object. The cached inverse
## is used instead of calculating it again. This function assumes that the 
## supplied matrix is invertible and therefore this is not checked by this 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x.inv <- x$getinverse()
        if(!is.null(x.inv)) {
                message("getting cached data")
                return(x.inv)
        }
        data <- x$get()
        x.inv <- solve(data, ...)
        x$setinverse(x.inv)
        x.inv
}