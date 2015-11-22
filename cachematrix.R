## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix, creates a special "matrix" 
## object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of the 
## special "matrix" that is returned by the makeCacheMatrix function above. 

## If the inverse has been calculated previously and the matrix is the
## same, cacheSolve will retrieve the inverse from the cache. 

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
        i
}