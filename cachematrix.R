## Write a short comment describing this function
## This function sets the value of the matrix
## sets the value of the matrix
## gets the value of the matrix
## sets inverse of the matrix
## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Function produces the inverse of the matrix. Checking if the inverse has been
## computed. If it has been computed it gets the result and skips the
# computation. If not it computes the inverse, and sets the value in the cache using
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
