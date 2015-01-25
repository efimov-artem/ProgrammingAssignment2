# This is Programming Assignment 2 of "R Programming" course by Johns Hopkins
# Bloomberg School of Public Health on coursera.org
# Copyright: Artem Efimov, 2015.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        set <- function(y) {
                # assume that the matrix supplied is always invertible, so
                # here is no any input data checking 
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(IM) InverseMatrix <<- IM
        getInverseMatrix <- function() InverseMatrix
        list(set = set,
             get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        IM <- x$getInverseMatrix()
        if(!is.null(IM)) {
                message("getting cached data")
                return(IM)
        }
        M <- x$get()
        IM <- solve(M, ...)
        x$setInverseMatrix(IM)
        IM
}
