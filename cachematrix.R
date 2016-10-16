## Functions which can cache the inverse of a matrix

## Function for creating a matrix object which is able to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mtx) m <<- mtx
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## Function for computing the inverse of the matrix returned by makeCacheMatrix. In case it's already computed, it's returned the inverse from the cache

cacheSolve <- function(x, ...) {
     
        inverseMatrix <- x$getmatrix()
        if (!is.null(inverseMatrix)) {
                message("inverse from the cache")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setmatrix(inverseMatrix)
        inverseMatrix
}
