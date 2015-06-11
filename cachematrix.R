## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = Matrix()) {
       inv <- NULL
       set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



cacheSolve <- function(x, ...) {
	## takes the matrix from the output of makeCacheMatrix function 
	##and inverse of the original matrix input to makeCacheMatrix()
        
	inv <- x$getinv()

	# If its already calculated,gets the data from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

	## Calculates the inverse

        data <- x$get()
        inv <- solve(data, ...)
        
	## sets the value in cache
	  x$setinv(inv)
        return(inv)
}