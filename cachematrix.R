## The following is a pair of function that cache and compute the
## inverse of a matrix

## This function creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(y = matrix()) {
        inverse <- NULL
        set <- function(x) {
            y <- x;
            inverse <<- NULL;
        }
        get <- function() return(y);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set,get = get,setinv = setinv,getinv = getinv))
}

## This function computes inverse of the special matrix
## returned by makeCacheMatrix.
## If the inverse has already been calculated,the matrix has not changed,
## then cacheSolve should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        inverse <- y$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        data <- y$get()
        inverse <- solve(data,...)
        y$setinv(inverse)
        return(inverse)
}
