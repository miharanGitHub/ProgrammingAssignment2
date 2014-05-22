## This file contains functions for matrix creation, storage 
## and inverse matrix calculation.
## Functions cache's the inverse matrix so there is no need to
## calculate it repeatedly if the inverse matrix already exists.

## This is a function for "special" matrix creation, which contains
## a list of functions. 

makeCacheMatrix <- function(Matrix=matrix()) {
	
	# Checking if the function argument is a matrix
	if (!is.matrix(Matrix)){
		message("argument is not matrix")
		return(Matrix)
	}
	# Initializing inverse matrix
	inverseMatrix<-NULL
	
	#set function 
	setMatrix<-function(y){
		# Check if the set value is a matrix
		if (!is.matrix(y)){
			message("argument is not matrix")
			return(y)
		}
		Matrix <<- y
		inverseMatrix <<- NULL
	}
	
	# get function
	getMatrix<-function() Matrix
	# set inverse matrix function
	setInverse<-function(inverse) {
		inverseMatrix <<- inverse	
	}
	# get inverse matrix function
	getInverse<-function() inverseMatrix

	list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)

}

## This function calculates the inverse of the "special" matrix, 
## which is given as a argument "x".
## If the inverse matrix already exists for the matrix "x"
## the inverse matrix is not calculated but the
## function returns already existing inverse matrix.
## Function assumes that the given "special" matrix is not 
## singular and invertible.

cacheSolve <- function(x, ...) {
		# Checking if the inverse exists
	    m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
		# get matrix
        Matrix <- x$getMatrix()
		# checking if the matrix is a square matrix
		if (ncol(Matrix)!=nrow(Matrix)){
		        message("Matrix is not squared matrix!")
				return(Matrix)
		}
		# calculate the inverse
        m <- solve(Matrix)
		# set the inverse
        x$setInverse(m)
        ## Return a matrix that is the inverse of 'x'
        m	
}

