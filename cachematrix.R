## I will write a pair of functions that cache the inverse of a matrix.
 
## 1. makeCacheMatrix
#This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
s<- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}

	get <- function()x
	setsolve<-function(solve) s <<- solve
	getsolve<-function() s
	getenv<- function() environment()
	list(set = set, get = get, setsolve=setsolve, getsolve = getsolve, getenv=getenv)

}

## 2. cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data,...)
	x$setsolve(s)
	s	## Return a matrix that is the inverse of 'x'
}
