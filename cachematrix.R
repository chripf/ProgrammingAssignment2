## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function()x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    inv<-solve(x)
    x$setinv(inv)
    inv
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    
    if(!is.null(m)){
        # activate for development
        message("getting cached data")
        return(m)
    }
}
