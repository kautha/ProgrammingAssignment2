
## Input argument of makeCacheMatrix is an invertible matrix. This function when 
##  used along with cacheSolve function, can cache its own matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        # x is an invertible matrix
        # initializing invert of matrix to NULL
        inv = NULL
        # setting matrix value to the value of inputted invertible matrix x
        # and its inverse to NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        # get function returns the matrix x
        get = function() x
        # setinverse sets a value to inverse matrix
        setinverse = function(inverse) inv <<- inverse 
        # getinverse returns the inverse matrix
        getinverse = function() inv
        # function returns list containing functions to set & get a matrix and 
        # its inverse.
        list(set=set, get=get, setinv=setinverse, getinv=getinverse)
}
# The function returns the cached inverse matrix. If it is not found in the 
# cached variable then it computes it and then returns.

cacheSolve <- function(x, ...) {
        ## x is a list of functions to get & set a matrix value and its inverse
        inv = x$getinv()
        
        # if the inverse value is not NULL, function returns the cached value
        if (!is.null(inv)) return(inv)
        
        # if the inverse value is NULL, its value is computed
        inv = solve(x$get(), ...)
        
        # sets the value of the inverse in the cache 
        x$setinv(inv)
        
        return(inv)
}
