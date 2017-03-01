# The following functions attempt to improve efficiency by not re-computing the inverse
# of a matrix, and instead returning a cached version a previously solved inversed matrix.

# function contains accessor like functions that cache a matrix and cache the inverse of the inverse matrix
# returns a list of the accessor functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y){
        # assign in the parent environment
        x <<- y
        m <<- NULL
    }

    get <- function(){
        x
    }

    setinverse <- function(z){
        m <<- z
    }

    getinverse <- function(){
        m
    }

    # return the accessor functions
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# computes and returns the inverse of a matrix created by the makeCacheMatrix.
# if the matrix inverse has already been computed, then a cached version is returned to avoid the computation cost
cacheSolve <- function(x, ...) {
    # check to see if we already computed a matrix from makeCacheMatrix
    m <- x$getinverse()

    if(!is.null(m)){
         message("Getting cached data")
        # if exists, no need to solve the inverse, return the cached value
        return(m)
    }

    # otherwise we need to compute the inverse using solve
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)

    return(m)
}
