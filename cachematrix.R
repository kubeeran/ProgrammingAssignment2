##The below function creates a special object that stores the invertible square
## matrix and cache's it's inverse 

## the makeCacheMatrix function creates a special matrix with the following functions,
## setting the value of the matrix, getting the value of the matrix,
## setting the inverse of the matrix, getting the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    set <- function(y){
         x <<- y
         k <<- NULL
    }
    get <- function() x
    setinverse <- function(inve) k <<- inve
    getinverse <- function() k
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function solves the inverse of the matrix created in the above 
## function. If the inverse is already created, it gets the value from cache and
## skip the computation. If not, it calculates the inverse and store it in cache 
## using setinverse function.

cacheSolve <- function(x, ...) {
        k <- x$getinverse()
        if(!is.null(k)) {
                message("getting cached data of the inverseed matrix")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinverse(k)
        k
}