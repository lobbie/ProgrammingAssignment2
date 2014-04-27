## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.  
## The functions cache the value of the matrix inverse so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 

## The first function, `makeCacheMatrix` creates a special "matrix", 
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	# initialise the inv variable with NULL value
	inv <- NULL
			
	# set & get value of the matrix
  	set <- function(y) {
  		x <<- y
        	inv <<- NULL
    	}
    	get <- function() x
    
    	# set & get value of the inverse
    	setinv <- function(inverse) inv <<- inverse
    	getinv <- function() inv
    	
    	# return the values of the matrix & its inverse to a list
    	list(set = set, get = get,
         	setinv = setinv,
         	getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## matrix has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {

	# Test if inverse is cached.  
	# If so return inv & exit function.				
	inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
                return(inv)
        }
        
        # If not, compute the matrix inverse using 
        # the solve function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
