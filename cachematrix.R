#makeCaheMatrix function creates a special matrix, which is a list containing a function to 4 purposes:
#1. Set the values of the matrix
#2. Get the values of the matrix
#3.Set the value of the inverse
#4.Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { x <<- y
                        m <<- NULL}
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set=set, get=get,setInv=setInv, getInv=getInv)
}


#cacheSolve function calculates the inverse of the special matrix returned by the function makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
    
        m <- x$getInv()
        if(is.null(m)==F) { message("getting cached data")
                                 return(m) }
        m <- solve(x$get(), ...)
        x$setInv(m)
        m
}
