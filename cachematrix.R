## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix defines a matrix object and sets and gets its data value and inverse matrix. Data and inverse value are defined in the cache. If no matrix is specified for the x optional argument, an empty matrix is initialized. 

makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <<- NULL
	set <- function(y){
		x <<- y
		inv_matrix <<- NULL
	}

	get <- function() x
	setinv <- function(inv) inv_matrix <<- inv
	getinv <- function() inv_matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve gets the cached inverse of matrix x. If it is not defined, it is calculated and stored in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinv()
        if (!is.null(inv_matrix)) {
        	message("Getting cached inverse matrix")
        	return(inv_matrix)
        }

        matrix <- x$get()
        inv_matrix <- solve(matrix,...)
        x$setinv(inv_matrix)
        inv_matrix
}
