## This file provides two functions that stores a matrix and computes its
## identity matrix.  If identity matrix is calculated in a previous run, it
## will return a cached result thereby saving computation time.

## creates a special matrix that sets and gets the value of the matrix and
## sets and gets the identity matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}

## This calculates the identity matrix set by the makeCacheMatrix function 
## If the special matrix has not changed, then the cached result will return.
## Otherwise it will calculate the identity matrix and store it into the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
