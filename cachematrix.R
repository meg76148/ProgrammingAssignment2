## Two functions to create a matrix and return the inverse of that matrix.

## makeCacheMatrix returns a list of functions that:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	# invmatrix stores the cached inverse matrix
	invmatrix <- NULL
	
	# set will set the value of matrix
	set <- function(y) {
		x <<- y
		invmatrix <<- NULL
	}

	# get the matrix x
	get <- function() x

	#set the inverse
	setinv <- function(inverse) invmatrix <<- inverse

	#get the inverse
	getinv <- function() invmatrix
	
	#return matrix with functions
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


# cacheSolve: Will compute the inverse of the matrix. 
#If the inverse is calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()

	#If inverse is calculated, return inv
	if (!is.null(inv)) {
		message("Get Cached Data")
		return(inv)
	}

	#Inverse is not yet calculated, so calculate it
	data <- x$get()
	inv <- solve(data, ...)

	#Cache inverse
	x$setinv(inv)

	#Return
	inv
}

        ## Return a matrix that is the inverse of 'x'