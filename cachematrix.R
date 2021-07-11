## the understanding of this has an two elements in which describes the first is the makeCacheMatrix and second is the CacheSolve.
## functions do able to calculate the makeCacheMatrix in which to contain the from provided all of the set, get, setInverse and the getInverse.
 
## to enable the makeCacheMatrix on creating and returning the list of  the functions.

makeCacheMatrix <- function(x = matrix()) {
 
        inv <- NULL
        set <- function(y) {
         
                original.matrix <<- y
                inverted.matrix <<- NULL
         
                }
        get <- function() {original.matrix}
        setInverse <- function(solve) {inverted.matrix <<- solve}
        getInverse <- function() {inverted.matrix}
         

        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
            }


## cacheSolve, which are able to calculate all of the inverse from the matrix in which been created through an makeCacheMatrix.
## it enables to possibly able on getting the inverse of the matrix that been stored in cache.
## it was created through the working environment and through an inverted value.

cacheSolve <- function(x, ...) {
 
        inverted.matrix <- x$getInverse()
        if(!is.null(inverted.matrix)) {
                message("retrieving cached data")
                return(inverted.matrix)
                }
 
        matrix <- x$get()
        inverted.matrix <- solve(matrix, ...)
        x$setInverse(inverted.matrix)
        inverted.matrix
 
        }
