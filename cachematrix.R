## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## Write a short comment describing this function
##This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(!is.null(z)) {
        message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setInverse(z)
        z
}
        
