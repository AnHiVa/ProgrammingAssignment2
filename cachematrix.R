## Creates a matrix which inverse can be solved and cached 

## This function creates the matrix into the environment of the same function
## And creates a list of the objects so they can be queried

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y 
      inv <<- NULL 
    }
    get <- function () x 
    setinverse <- function(solve) inv <<- solve 
    getinverse <- function() inv 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function solves the inverse for the matrix created in the previous function and caches it
## If a value is already created for the mean of the matrix, gives the stored value

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
