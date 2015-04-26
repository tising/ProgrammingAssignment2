## The functions in this script optimize the computation of the 
## inverse of a square matrix. The operation is optimized by 
## enabling the caching of the matrix inverse so that the operation
## does not have to be repeated when the inverse is requested again
## on the same matrix.


## The makeCacheMatrix function creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## The cacheSolve function computes the inverse of the special
## matrix returned by the makeCacheMatrix function retrieving
## the inverse from the cache when it is already calculated.

cacheSolve <- function(x, ...) {

        s <- x$getinverse()
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        data <-x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
