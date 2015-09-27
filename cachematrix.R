## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                           # Define function to set the value of the matrix
        set <- function(y) {                                  # inv will store the cached inverse matrix
                x <<- y                                       # Set the value
                inv <<- NULL                                  # Clear the cache
        }
        get <- function() x                                   # Define function to get the value of the matrix
        setinverse <- function(inverse) inv <<- inverse       # Define function to set the inverse. This is only used by getinverse()
        getinverse <- function() inv                          # when there is no cached inverse
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)   # Return a list with the above four functions
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                            # This fetches the cached value for the inverse
        if(!is.null(inv)) {                              # If the inverse is already calculated, return it
                message("getting cached data.")
                return(inv)
        }                                                # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)                                # Cache the inverse
        inv                                              # Return it
}

## Test
## a <- makeCacheMatrix ( matrix (1:4,2,2))
## cacheSolve (a)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
