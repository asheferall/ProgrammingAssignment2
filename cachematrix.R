# Provides functions to set and get the matrix and invese of the matrix. 
# Also caches the invese of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	
	set <- function(y) { 
		x <<- y
		inverseMatrix <<- NULL
	}
	
	get <- function() x
	
	setInverseMatrix <- function(m) inverseMatrix <<- m
	
	getInverseMatrix <- function() inverseMatrix
	
	list(set = set,
		 get = get,
		 setInverseMatrix = setInverseMatrix,
		 getInverseMatrix = getInverseMatrix)
}


# Checks whether the inverse has been calculated if not calculates it and stores it in the cacheSolve
# Returns the invese matrix 

cacheSolve <- function(x, ...) {
	m <- x$getInverseMatrix()
	
	if (!is.null(m)) { 
		message("Getting Cached Invese Matrix")
		return(m)
	}
	
	data <- x$get()
	m <- solve(data)
	x$setInverseMatrix(m)
	
	m
}
