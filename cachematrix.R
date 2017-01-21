## The makeCacheMatrix function will create a special matrix object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cached_inv<-NULL
    set<-function(y){
        x<<-y
        cached_inv<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) cached_inv<<-solve
    getinverse<-function() cached_inv
    list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolved function will compute the inverse of the matrix returned by
## makeCacheMatrix function if the matrix is new and save the value to cache.
## Otherwise it'll retrieve the inverse of the matrix from cache.

cacheSolve <- function(x, ...) {
    cached_inv<-x$getinverse()
    if(!is.null(cached_inv)){
        message ("Getting cached inverese matrix")
        return(cached_inv)
    }
    data<-x$get()
    cached_inv<-solve(data, ...)
    x$setinverse(cached_inv)
    cached_inv
}
