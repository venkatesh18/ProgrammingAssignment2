## This program calculates the inverse of a square matrix and caches it in a special "matrix"
## object so that the inverse may be used at a later stage without having to recompute it. The
## program assumes that the input matrix is square. It returns an error message if the input
## matrix is not square.


## This function creates a special "matrix" object to cache the inverse of the input matrix. This
## special "matrix" object is a list containing funcions to set the value of a matrix, get the value
## of the matrix, set the value of inverse of a matrix and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get the value of the marix
        get <- function() x
        
        ## set inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ## get inverse value
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get inverse of x
        inv <- x$getinverse()
        
        ## If inverse exists, return the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise, get input data and calculate inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## set the inverse value
        x$setinverse(inv)
        inv
}
