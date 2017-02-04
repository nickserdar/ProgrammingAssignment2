## The combination of these two functions will compute the inverse of any invertible matrix.
## If the inverse was already calculated, the inverse will be retrieved from cache data. 
## If the inverse has not been calculated, the function will compute the inverse using the solve function.

## This function will create a list that contains the elements 
## defined in the fiction. (set matrix , get matrix, set inverse, get inverse)

makeCacheMatrix <- function(x = matrix(), ...) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}  


## This function will compute the inverse of the matrix. If the inverse was already computed, 
## the function will retrieve its inverse from cache - skipping the computation.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) 
        x$setinverse(m)
        m
}
