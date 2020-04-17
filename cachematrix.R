## This is a pair of functions that cache and compute 
## inverse of a matrix

## This function creates a special "matrix" object
## that cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        set <- function(x){
            m <<- x;
            inverse <<- NULL;
        }
        get <- function() return(m);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set,get = get,setinv = setinv,getinv = getinv))
}

## This funv=ction computes inverse of the special
## matrix returned by makeCacheMatrix.
## If inverse has already been calculated,and matrix has not changed,
##then cacheSolve should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        inverse <- m$getinv()
        if(!is.null(inverse)){
                message("Getting cached data...")
                return(inverse)
        }
        data <- m$get()
        inverse <- solve(data,...)
        m$setinv(inverse)
        return(inverse)
}
