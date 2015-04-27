## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## To create a special matrix object that can cache its inverse
## this function including four sub-functions (set,get,setemean,getmean
makeCacheMatrix <- function(x = matrix()) {
	## x is a square invertible matrix
	m <- null
	##set the list value to the memory
	set <- function(y) {
		x <<- y
		m <<- null
	}
	##get the value of the vector from the memory
	get<- function() x
	##set the value of the mean to the variable m
	setmean <- function(mean) 
		m<<-mean
	##get the value of the mean stored in the variable m
	getmean <- function() m
	##make a list with the functions
	list(set=set, get=get,
		setmean=setmean, getmean=getmean)
}


## Write a short comment describing this function
## This computes the inverse of the matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	## if the inverse has been calculated, return a value of m
	m <- x$getmean()
	if(!is.null(m)) {
		message("gettting cached data")
		return(m)
	}
	##get a vertor stored in x
	data <- x$get()
	##calculate the mean of the vector and store in m
	m <- mean (data, ...)
	##store the value of the mean into m 
	x$setmean(m)

	## retrieve the inverse from the cache
	m
}
