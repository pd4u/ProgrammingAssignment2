# =============================================================
# makeCacheMatrix
#
# Purpose : This function creates a special object that 
#           encapsulates caching of the inverse of a matrix
# =============================================================

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL 
    }
    
    get <- function()
    {
        x
    }
    
    setinverse <- function(inverse)
    {
        inv <<- inverse
    }
    
    getinverse <- function()
    {
        inv
    }
    
    # Representation of the "special" object
    # Encapsulates inverse data and provides methods to access the underlying data
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# =============================================================
# cacheSolve
# Purpose : This function computes the inverse of a matrix
#           and saves in the specialized "encapsualation" object.
#           On subsequent requests for the inverse of a matrix, 
#           it returns the cached value
# =============================================================

cacheSolve <- function(x, ...)
{
    i <- x$getinverse()
    
    
    if(!is.null(i))
    {
        message("getting cached data..")
        return(i)
    }
    
    
    #message("computing inverse")
    mx <- x$get()
    i <- solve(mx, ...)
    x$setinverse(i)
    
    
    
    i
}
