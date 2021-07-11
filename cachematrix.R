## the understanding of this has an two elements in which describes the first is the makeCacheMatrix and second is the CacheSolve.
## functions do able to calculate the makeCacheMatrix in which to contain the from provided all of the set, get, setInverse and the getInverse.
 
## to enable the makeCacheMatrix on creating and returning the list of  the functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
         
                }
        get <- function() (x)
        setInverse <- function(inverse) (inv <<- inverse)
        getInverse <- function() (inv)
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
            }


## cacheSolve, which are able to calculate all of the inverse from the matrix in which been created through an makeCacheMatrix.
## it enables to possibly able on getting the inverse of the matrix that been stored in cache.
## it was created through the working environment and through an inverted value.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("retrieving cached data")
                return(inv)
                }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        }
