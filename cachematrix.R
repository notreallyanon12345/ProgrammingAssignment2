## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as an argument and creates a list
## of functions that set the matrix, get the matrix, set the inverse
## of the matrix, and get the inverse of the matrix

## cacheSolve takes the matrix output of makeCacheMatrix and gets
## the inverse if it is already cached, and sets it if it is not
## already cached

## makeCacheMatrix caches a matrix along with its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve returns an inverse of the input matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
