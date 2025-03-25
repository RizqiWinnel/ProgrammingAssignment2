## These functions create a special matrix object that can cache its inverse
## and compute the inverse efficiently by retrieving from the cache if available.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y){
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinv <- function(solve) n <<- solve
        getinv <- function() n
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache instead of recomputing it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinv()
        if(!is.null(n)) {
                message("getting cached inverse")
                return(n)
        }
        mat <- x$get()
        n <- solve(mat, ...)
        x$setinv(n)
        n
}
