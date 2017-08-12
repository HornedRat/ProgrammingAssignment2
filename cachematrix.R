## The two functions are able to compute an inverse matrix of a given matrix
## The functions cache an inverse matrix, so if the input matrix has not been changed,
## the function retrieves a previously computed solution, instead of performing
## the inversion again

## This function creates an object, containing 4 functions£
## to set the input matrix, to get it, to set an inverse matrix and to get it.
## The function allows for storing the computed inverse matrix

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


## This function computes the inverse matrix, OR, if the data
## has not changed, retrieves the previously computed solution,
## therefore speeding up the process.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
