## create a special matrix object, 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) m <<-inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()

    ## If the inverse was already been calculated, cache and return it
    if (!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    } else {
        m <- solve(x$get())
        x$setinverse(m)
        return(m)
    }
}