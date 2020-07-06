## Creating a matrix that caches its inverse
makeCacheMatrix <- function (x = matrix()) {
        inv <- NULL
        ## Setting the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Getting the matrix
        get <- function() x
        ## Setting the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        ## Getting the inverse of the matrix
        getinverse <- function() inv
        ## Returning a list of above results
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Calculating the inverse of the matrix returned from "makeCacheMatrix" function.
## If the inverse has already calculated, "chachesolve" function will get the inverse result from the cache.  
cacheSolve <- function(x, ...) {
        ## Returning a inverse matrix of x
        inv <- x$getinverse()
        ## Returning the inverse in the cache if its already calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        ## Computing the inverse using solve
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
