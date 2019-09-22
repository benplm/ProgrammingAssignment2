## Author: Benedetto Polimeni

## Date:   2019-09-22

## Title:  Coursera "R Programming" (JHU Data Science specialization),
##         assignment 2

## The two functions defined below return the inverse of an invertible matrix
## and cache the result to avoid computing it repeatedly.


## This function takes a matrix as input and store it in a special "matrix" 
## object. After computing the inverse of the matrix, it will cache the inverted
## matrix as well.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) inv <<- solve
        
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function function computes the inverse of an invertible matrix stored in
## the special "matrix" object obtained with makeCacheMatrix and cache the
## result in the same object. If the inverse has already beed calculated for the
## same matrix, it retrieves the result from the cache rather than computing it
## again.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv

}
