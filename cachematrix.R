## The two functions below are used to create a special object that stores a matrix and caches its inverse
## so that when needed again, it can be looked up in the cache rather than recomputed.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## This special object is really a list containing a function to:
## - set the value of the matrix;
## - get the value of the matrix;
## - set the value of the inverse;
## - get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmtrx <- NULL

	set <- function(y) {
		x <<- y
		invmtrx <<- NULL
	}
	
	get <- function() x

	setInverse <- function(inverse) invmtrx <<- inverse

	getInverse <- function() invmtrx

	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve will 
## retrieve the inverse from the cache and skip the computation. Otherwise, it will calculate the inverse of the 
## data and set the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtrx <- x$getInverse()
        
	if(!is.null(invmtrx)) {
		message("getting cached data")
		return(invmtrx)
	}
	
	data <- x$get()
	invmtrx <- solve(data,...)
	x$setInverse(invmtrx)
	invmtrx
}
