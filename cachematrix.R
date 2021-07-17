## Put comments here that give an overall description of what your
## functions do

## this function will make an inverse of x

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function() i <<- solve(x)
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## this function will inverse the matrix returned by function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

