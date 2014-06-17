## This function creates a special matrix to manage the cache of the 
## inverse of a matrix.
## The special matrix has got 4 enclosed functions to manage the cache
## The 4 enclosed functions are:
## 	A) set
##	B) get
##	C) setInverseMatrix
##	D) getInverseMatrix
## To use this function the following steps are to be followed
##	1. create a matrix that can be inversed, iX <- matrix(c(1:4), nrow=4, ncol=4)
##	2. create the special matrix, spCM <- makeCacheMatrix()
##	3. set the matrix, spCM$set(iX)
##	4. execute the cacheSolve(spCM)
##	FUNCTION STARTS HERE
makeCacheMatrix <- function(x = matrix()) {
	#Set the cache of the inverse matrix to NULL
	inverseM <- NULL
	
	#Set the special matrix vector with the input matrix
	#The function doesn't validate that whether the input matrix inversible
	#The assumption is that the input matrix is inversible
	set <- function(mY) {
		message ("Inside set")
		
		storeInverse <<- mY
		inverseM <<- NULL
	}

	#Returns the matrix, that is to be inversed
	get <- function() {
		message ("Inside get")
		
		storeInverse
	}

	#This sets the calculated inverse in the cache
	setInverseMatrix <- function (inverseP) {
		message ("Inside setInverseMatrix ")
		
		inverseM <<- inverseP
	}

	#This returns the calculated inverse matrix from the cache
	getInverseMatrix <- function (inverseP) {
		message ("Inside getInverseMatrix ")
		
		inverseM
	}

	#Creates a list of functions associated with the special matrix
      list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}
##	FUNCTION ENDS HERE

## This function calculates the inverse matrix if the inverse of the matrix
## Before calculating the inverse of the matrix, it checks the cache
## If the inverse is available in the cache, then it will return the 
## stored inverse
## This function depends on makeCacheMatrix for creating a special matrix
## to help with the caching
## This function also stores the inverse of the matrix in the cache if the
## the inverse is not already available in the cache

##	FUNCTION STARTS HERE
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		message ("Inside cacheSolve ")
		
		iMatrix <- x$getInverseMatrix()
		
		message ("after getInverseMatrix")
		
		if(!is.null(iMatrix)) {
			message ("getting cached inverse matrix")
			return(iMatrix)
		}
		
		message ("before get")
		
		xMatrix <- x$get()
		
		message ("after get")
		
		iMatrix <- solve(xMatrix)
		
		message ("after solve")
		
		x$setInverseMatrix(iMatrix)
		
		message ("after setInverseMatrix")
		
		iMatrix
}
##	FUNCTION ENDS HERE
