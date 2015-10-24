makeCacheMatrix <- function(x = matrix()) {
        
        # Let x be a square and invertible matrix. (Otherwise R will throw an error).
        # makeCacheMatrix returns a list as input for cacheSolve.
        
        # "inv": inverted matrix 
        inv = NULL
        
        # "set" puts y to x outside of the current environment.
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # "get" returns matrix
        get = function() x
        
        # "setinv" puts inv outside of the current environment
        setinv = function(inverse) inv <<- inverse 
        
        # "getinv" returns inv
        getinv = function() inv
        
        # Returning the list:
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# ------------

cacheSolve <- function(x, ...) {
        
        # Computes the inverse of the special "matrix" 
        # returned by makeCacheMatrix above. 
        # If the inverse has already been calculated 
        # (and the matrix has not changed), then the cacheSolve 
        # retrieves the inverse from the cache.
        
        # x is the output list of makeCacheMatrix()
        
        # get inv
        inv = x$getinv()
        
        # if inv is not NULL (if calculated before): get it from the cache and returns it
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # if inv still is NULL (not calculated by now): the inverse is calculated
        data = x$get()
        inv = solve(data, ...)
        
        # the calculated inverse is stored
        x$setinv(inv)
        
        # ready!
        return(inv)
}

