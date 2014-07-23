## makeCacheMatrix
##      Creates an object wrapping a matrix and then caching it's inverse after the first calculation.
## cacheSolve
##      Uses the object created by makeCacheMatrix(x) to solve the matrix and uses cached data, if possible.


## Creates an object providing functions for storing a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set<-function(y){   # Sets a new value for the matrix and resets the cached value.
        x<<-y
        i<<-NULL
    }
    get <- function()x  # Retrieve the wrapped matrix
    
    setinv <- function(inv) i <<- inv   # Store the inverse matrix for cached access.
    getinv <- function() i              # Retrieve the cached inverse matrix.
       
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Uses an object created by makeCacheMatrix(x) to either return a cached inverse matrix
## or calculate the inverse and cache it, then returning the calculated inverse matrix.  

cacheSolve <- function(x, ...) {    
    inv<-x$getinv() # Try to access the cached inverse and return it, if found.
    
    if(!is.null(inv)){
        # activate for development
        message("getting cached data")
        return(inv)
    }
    
    origMatrix <- x$get()   # fetch original matrix
    inv<- solve(origMatrix) # solve it
    x$setinv(inv)           # and cache it
    inv
    
}
