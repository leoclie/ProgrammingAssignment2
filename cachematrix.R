## Put comments here that give an overall description of what your
## functions do

## The following function creates a matrix object that can cache its inverse
## It basically sets matrix, gets matrix, sets inverse and gets inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invers) inv <<- invers
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the matrix produced by makeCacheMatrix above.
## Once the inverse is computed, then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}

