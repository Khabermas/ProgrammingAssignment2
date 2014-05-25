## Put comments here that give an overall description of what your
## functions do

## From a square matrix x, Creates a 'special matrix' (list containing matrix and functions) that can be cached.  
## That special matrix can serve as input for the CacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
    
    x.inverse <- NULL
## Defines functions to get and set cached matrix inverse
    set <- function(y)  {
        x <<- y
        x.inverse <<- NULL
    }
    
    get <- function() x
 ## Inverts the matrix using solve() function   
    set.inverse <- function(solve) x.inverse <<- solve
    get.inverse <- function() x.inverse
    
    list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}


## Calculates the inverse of the special matrix returned by makeCacheMatrix().
## If inverse has previously been calculated, returns the cached matrix inverse
## without incurring duplicate calculational expense.

cacheSolve <- function(x, ...) {
    x.inverse <- x$get.inverse()
    if(!is.null(x.inverse)) {
        message("getting cached matrix inverse")
        return(x.inverse)
    }
    ## Invert matrix if x.inverse not already calculated
    x.uninverted <- x$get()
    x.inverse <- solve(x.uninverted)
    x$set.inverse(x.inverse)
    x.inverse
}
