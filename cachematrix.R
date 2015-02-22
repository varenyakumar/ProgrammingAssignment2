##Coursera - R Programming
##Programming Assignment 2
##submitted 2/22/2015,

##This assignment is about learning how to cache potentially time-consuming operations
##so the operations needn't be performed everytime, but just the cahced data can be used

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix function above. If the inverse has already been calculated
##(and the matrix has not changed), then the cacheSolve function should retrieve
##the inverse from the cache.

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

#This code is similar to the example provided for the assignment
#However, the 'getting cached data' doesnt happen only if the matrix is changed
#To be fully accurate this shouldn't happen even when the parameters for the matrix are changed.
