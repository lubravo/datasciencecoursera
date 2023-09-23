## File name: cachematrix.R
## This script contains the following functions:
## makeCacheMatrix
##      Receives invertible matrix with argument "x" as input
##      Returns a "special" list with the following functions as elements:
##      'set'  sets the value of the "x" matrix in the cache
##      'get'  returns the inverted matrix of "x"
##      'setsolv' sets the inverted matrix in the cache
##      'getsolv' returns the current inverted matrix (return sa NULL value if the matrix inversion calculation has not been calculated)
############
## Author: Luis Bravo
## Version date: September 23rd 2023
## Version id: 1.0
############
## cacheSolve

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	    solv_mx <- NULL
	    set <- function(y) {
	    	x<<- y
	    	solv_mx <<- NULL
	    }
	    get <- function() x
	    setsolv<- function(solv) solv_mx <<- solv
	    getsolv<- function() solv_mx
	    list(set = set, get = get,
	          setsolv = setsolv,
	          getsolv = getsolv)

}


## The cacheSolve function returns the inverted matrix of the matrix element in a special list.If the current cached inverted matrix is empty
## it makes the calculation using the "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solv_mx <- x$getsolv()
        if(!is.null(solv_mx)) {
        	message("getting cached data")
        	return(solv_mx)
        }
        data<- x$get()
        solv_mx<- solve((data))
        x$setsolv(solv_mx)
        solv_mx
}
