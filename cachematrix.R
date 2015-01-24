## These functions implement caching for computing the inverse of a matrix.

## Coursera R Programming, class rprog-010
## Programming Assignment 2
## Due 25 January 2015

## For this assignment, we assume the matrix supplied is always invertible.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # inv holds the cached inverse
        inv <- NULL

        # The set function handles initialization
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # The get function returns the matrix
        get <- function() x

        # The setinverse function saves a cached value for the inverse
        setinverse <- function(inverse) inv <<- inverse

        # The getinverse function returns the cached value (or NULL)
        getinverse <- function() inv

        # Available functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not
## been changed), retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (is.null(inverse)) {
                # Cached result not available so calculate and save in cache
                inverse <- solve(x$get(), ...)
                x$setinverse(inverse)
        }
        inverse
}

