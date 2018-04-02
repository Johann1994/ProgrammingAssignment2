## Caching the inverse of a Matrix


## Creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(SetMat)
        {
                x <<- SetMat
                inv <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        ## Define list that objects are subsettable
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix and return 
##the cached value when the calculation allready has been done


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        ## Check if a value has been cached
        if(!is.null(inv)){
                message("Message: Using Inversed Cached data matrix")
                inv
        }
        ##else make calculation of inversed data
        data <- x$get()
        inv <- solve(data,...)
        
        ##cache data
        x$setinvers(inv)
        inv
}
