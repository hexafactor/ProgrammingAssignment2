## ------ CACHED MATRIX CONTAINER ------
## These functions create a container for a matrix with cached inverses,
## i.e. if the inverse is calculated, it is stored and taken from the
## cache next time it is requested (to reduce time needed).


## ------ makeCacheMatrix ------
## This function creates the container, including all its functions (four
## functions are part of the container). It can be initiated with providing
## a matrix as an argument, but the matrix may also be set later.

makeCacheMatrix <- function(x = matrix()) {
        # the variable for the inverse is initiated as empty (for now)
        inverse <- NULL
        
        # (a)
        # the set functions allows to set the matrix contained in the 
        # container at a later point (it also resets the inverse to empty
        # again if the value of the matrix changes)
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # (b)
        # returns the matrix itself
        get <- function() x
        
        # (c)
        # sets the inverse for this container (is called from 'cacheSolve')
        setinverse <- function(inv) inverse <<- inv
        
        # (d)
        # returns the variable containing the inverse (or NULL if it has 
        # not yet been calculated)
        getinverse <- function() inverse
        
        # simply a list containing the 4 functions defined above (a, b, c, d)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ------ cacheSolve ------
## This function returns the inverse of an element created with the function
## 'makeCacheMatrix' (see above), which is a container for a matrix and if
## already calculated its inverse

cacheSolve <- function(x, ...) {
        # asks for the saved variable 'inverse' in the container passed as
        # an argument (can also be NULL if not yet calculated)
        inverse <- x$getinverse()
        
        # if it has already be calculated (not NULL), it writes a message
        # and returns the inverse from the cache (exiting the function)
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # if the inverse has not yet been calculated it is calculated here
        # and then set in the container (so it can be retrieved from cache
        # nest time)
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        
        # the inverse is returned from the function
        inverse
}

## ------ TESTING CODE ------
## the code below can be used to test the functions defined above
## (one use case)
## it is not needed for the functionality, only for testing purposes

# set a test matrix note to be invertible
#   4  7
#   2  6
test_matrix <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)

# creating the cached matrix object from the matrix defined above
testinv_matrix <- makeCacheMatrix(test_matrix)

# get the inverted matrix (first time, not from cache)
# note: solution is  0.6 -0.7
#                   -0.2  0.4
cacheSolve(testinv_matrix)

# get the inverted matrix again (second time, this time from cache)
cacheSolve(testinv_matrix)
