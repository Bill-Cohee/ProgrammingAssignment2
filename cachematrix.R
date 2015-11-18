## Put comments here that give an overall description of what your
## functions do
## This R file contains functions that will make it more efficient to find the inverse of a large matrix.
## makeCacheMatrix creates a matrix that has it's inverse stored in a cache
## cacheSolve creates a cached version of the inverse if it does not exist.  If it does exist, solving for the inverse again
## is skipped and the previously generated cached inverse is returned.

## Write a short comment describing this function
## given an invertible square matrix, create a "special" matrix who's inverse is cacheable
## example: 
## mymatrix<- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
## myspecialmatrix<-makeCacheMatrix(mymatrix)
##
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the cache to empty
    m<-NULL
    set<-function(y){
        x<<-y
        ##empty the cache when the matrix is changed.
        m<<-NULL
    }
    ## get the original matrix
    get<-function() x
    ##set the cached inverse
    setinverse<-function(solve) m<<- solve
    ##get the cached inverse
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## Write a short comment describing this function
## given a "special" matrix created by makeCacheMatrix, return it's inverse.  
## if the inverse is cached, the cached version will be returned.  If the inverse is not cached,
## create the inverse and store it in the cache.
## example:
## mymatrix<- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
## myspecialmatrix<-makeCacheMatrix(mymatrix)
## cacheSolve(myspecialmatrix)
## 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## first, check if cached...
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        ## we are done
        return(m)
    }
    ## if here, not cache hit so solve for the inverse and save it in cache.  Return the inverse.
    ## fetch the original matrix
    matrix<-x$get()
    # solve for the inverse of the matrix
    m<-solve(matrix)
    # update the cache with the inverse
    x$setinverse(m)
    ## return the inverse
    m
}