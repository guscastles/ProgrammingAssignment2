## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Caches a matrix and its inverse by using the appropriate
# functions get, set (the original matrix) and 
# getinverse, setinverse (for the inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
	m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m_inverse <<- inverse
        getinverse <- function() m_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
# Receives an object x with a list of cache functions
# and possibly cached objects (matrix and its inverse)
# and returns the inverse matrix, calculating it if
# not in the cache object.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        original_matrix <- x$get()
        m <- solve(original_matrix)
        x$setinverse(m)
        m
}
