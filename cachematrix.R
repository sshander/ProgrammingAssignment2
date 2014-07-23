## These functions will calculate the inverse of a given matrix, X so that when multiplied by X, 
## the identity matrix is the output.

## This function specifically makes a list of outputs related to the input X.  First it sets initial values
## for its list outputs.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function pulls the output from the previous function under x[getinverse].  If the value is there,
## it will print the message "getting cached data", then the inverese.  If the value is not previouslyg saved,
##it will need to calculate the inverse using the solve() function, then storing it, and finally retrieiving it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}        ## Return a matrix that is the inverse of 'x'

