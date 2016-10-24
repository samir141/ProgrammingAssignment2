## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse


## The first function, makeCacheMatrix returns a list containing a function to
## 1.set the matrix 2.get the matrix 3.set the inverse of the matrix
## 4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function finds the inverse of the special "matrix" created
## with the above function. However it first checks the catche, if not available
## then only calculates the inverse


cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
