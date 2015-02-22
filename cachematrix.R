## Create a special "matrix" object that can set and cache a matrix and
## its inverse.
## Precondition: `x` must be an invertible square matrix
##
## Examples: x <- matrix(1:4,2,2) for a 2x2 invertible matrix
##           x <- matrix(c(1,0,5,2,1,6,3,4,0),3,3) for a 3x3 invertible matrix
##           x <- matrix(c(3,1,4,5,0,2,0,0,2,0,6,2,-1,-2,-3,0),4,4))
makeCacheMatrix <- function(x = matrix()) {
    ## HOld the inverse of the matrix
    inv <- NULL
    
    ## Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Function to retrieve the matrix
    get <- function() x
    
    ## Function to set the inverse of matrix
    setinv <- function(inverse) inv <<- inverse
    
    ## Function to retrieve the inverse of matrix
    getinv <- function() inv
    
    ## List containing functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Compute the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated
## then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Try retrieving the inverse from cache
    inv <- x$getinv()
    
    ## If the inverse already exists in cache return it
    if(!is.null(inv)) {
        message("Retrieving cached inverse data")
        return(inv)
    }
    
    ## Otherwise retrieve the matrix
    data <- x$get()
    
    ## Calculate the inverse of matrix
    inv <- solve(data, ...)
    
    ## Cache the inverse of matrix 
    x$setinv(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
