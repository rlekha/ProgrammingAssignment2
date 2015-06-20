## This file contains a pair of functions:makeCacheMatrix and cacheSolve 
## that can cache the inverse of a matrix
 
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	inverse_matrix <- NULL

  	set <- function(y) {
   	 	x <<- y
		inverse_matrix <<- NULL
 	 }

	get <- function() x

  	setminverse <- function(minverse) inverse_matrix  <<- minverse
	getminverse <- function() inverse_matrix 

  	list(set = set, get = get,
       	setminverse = setminverse,
       	getminverse = getminverse)

}


## This function calculates the inverse of a matrix object created
## using the makeCacheMatrix function.If the inverse of matrix
## has been calculated previously,then this function returns it
## from the cache of the "matrix" object,else it is calculated
## afresh and then stored in the cache of "matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Check if the inverse exists in cache 
	matrix_inverse <- x$getminverse()

	## Inverse exists.Return from cache
  	if(!is.null(matrix_inverse)) {
    		message("getting cached data")
    		return(matrix_inverse)
  	}
  	
	## Inverse does not exist in cache.Calculate the inverse
	matrix <- x$get()
  	matrix_inverse <- solve(matrix, ...)
	
	## Store the calculated inverse in cache
  	x$setminverse(matrix_inverse)

	## Return the calculated inverse of matrix
  	matrix_inverse

}
