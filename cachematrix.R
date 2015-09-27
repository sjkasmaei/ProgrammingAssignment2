## The following are a pair of functions that computte andcache the inverse of
## a matrix.

## The function "makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inversed matrix
## 4.get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The "cacheSolve" function computes the inverse of the special "matrix"
## created with the "makeCacheMatrix" function. It first checks to see if the
## inverse has already been computed. If so, it gets the inverse matrix from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache.
## Finally it returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
