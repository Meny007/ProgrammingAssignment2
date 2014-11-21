## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse property
                m <- NULL

    	## Method to set the matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }

	## Method the get the matrix
                get <- function() x
    		## Return the matrix

    	## Method to set the inverse of the matrix
		setmatinv <- function(solve) m <<- solve

    	## Method to get the inverse of the matrix
		getmatinv<- function() m

	## Return a list of the methods
		list(set = set, get = get,
                setmatinv = setmatinv,
                getmatinv = getmatinv)

}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
## Using the assumption that matrices used are all squared invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatinv()
        
   	## Just return the inverse if its already set
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
    	## Get the matrix from our object
	data <- x$get()
       
	## Calculate the inverse using matrix multiplication
	m <- solve(data, ...)


    	## Set the inverse to the object
	x$setmatinv(m)
        m
}
