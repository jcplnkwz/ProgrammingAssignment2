## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix is a function that stores a list of functions: : set, get, setinv, getinv.
## get is a function that returns the matrix stored in the main function. Doesn't require any input.
## set is a function that changes the matrix stored in the main function.
## setinv and getinv store the value of the input in a variable inv into the main function makeCacheMatrix (setinv) and return it (getinv).

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}			
		get <- function() x
		setinv <- function(solve) inv <<- solve
		getinv <- function() inv
		list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## cacheSolve verifies the value inv, stored previously with getinv, exists and is not NULL. If it exists in memory, it simply returns a message and the value inv. If the value inv it doesn't exist in memory, data gets the matrix stored with makeCacheMatrix, calculates the inverse of the matrix and x$setinv(inv) stores it in the object generated with makeCacheMatrix.

## Input of cacheSolve is the object where makeCacheMatrix is stored

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		if(!is.null(inv)) {
				message("Inverse of this matrix already exists in memory, so I'm getting cached data")
				return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv

        ## Return a matrix that is the inverse of 'x'
}
