## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {  ## Where x is a square invertible matrix
	
	inv = NULL
	
	set = function(y){
		x<<-y  ## The '<<-' sign asigns variables to those from different environments
		inv<<- NULL
	}  ## Sets matrix
	
	get = function() x  ## Gets matrix
	
	setinv = function(inverse) inv <<- inverse  ## Sets inverse of matrix
	
	getinv = function() inv  ## Gets inverse of matrix
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
	
	inv = x$getinv()  ## Gets value set up by using output from prior function
	
	if(!is.null(inv)){  ## Determines if inverse was calculated before
		message("getting cached data")
		return(inv)  ## If calculated: skip calculation and get value
	}
	
	mat.data = x$get()
	inv = solve(mat.data, ...)  ## Otherwise: calculate and get value
	
	x$setinv(inv)  ## Sets final value into cache
	
	return(inv)  ## Final output
}
