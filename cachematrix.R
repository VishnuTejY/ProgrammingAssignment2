## The following is pair of function that cache and compute inverse of matrix

## This function creates special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        inverse<-NULL
        set <- function(x){
                m <<- x;
                inverse<<-NULL;
                }
        get <- function() return(m);
        setinv <- function(inv) inverse<<-inv;
        getinv <- function() return(inverse);
        return(list(set=set,get=get,setinv=setinv,getinv=getinv))
}


## This function computes inverse of special matrix returned by makeCacheMatrix.
## If inverse has already been calculated,and matrix has not changed,
##then cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- m$getinv()
        if(!is.null(inverse)){
                message("Getting cache data..."")
                return(inverse)
        }
        data <- m$get()
        inverse <- solve(data,...)
        m$setinv(inverse)
        return(inverse)
}
