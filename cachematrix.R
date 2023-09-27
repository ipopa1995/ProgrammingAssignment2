####################################################################################################

# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
#
# Computing the inverse of a square matrix can be done with the 
# solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#
# For this assignment, assume that the matrix supplied is always invertible.
####################################################################################################


# The `makeCacheMatrix` function is creating a special "matrix" 
# object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        # initiate the inverse property
        m <- NULL

        # value to set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # value to get the matrix
        get <- function() {
                x
        }

        # value to set the inverse of the matrix
        set_inverse <- function(inverse){
                m <<- inverse
        }
        
        # value to get the inverse of the matrix
        get_inverse <- function(){
                m
        }
        
        # return a list of the functions
        list(set = set, get = get,
                setinverse = set_inverse,
                getinverse = get_inverse)

}


# The `cacheSolve` function takes a special "matrix" object 
# `x` as input and computes the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # get the inverse matrix
        m <- x$getinverse()

        # condition to check if the inverse matrix is already calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # get the matrix
        data <- x$get()

        # use the solve function to get the inverse matrix
        m <- solve(data, ...)

        # set the inverse matrix
        x$setinverse(m)
        m
}
####################################################################################################
x <-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(x)