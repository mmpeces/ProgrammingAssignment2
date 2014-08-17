## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##cacheSolve function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.



## makeCacheMatrix is a list containing a funtion to:
##1. sets the value of the matrix
##2. gets the value of the matrix
##3. sets the value og the inverse matrix
##4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve function calculates the inverse matrix of the special "matrix" created with makeCacheMatrix. 
##However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the 
##inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix
##of the data and sets the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
## Return a matrix that is the inverse of 'x'
