
## (c) 2014 Roland S. Labana
## These two funcitons are for calculating and returning the inverse of a matrix.
## A cache is used to store the calculated inverse of the matrix so it does
## not have to be caluculated each time.  The first function creates the cache
## The second function either calculates the inverse of the matrix if it's
## not available in the cache (and stores it in the cache), or it returns
## the cached inverse of the matrix.


## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse of the matrix
## get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseM) m <<- inverseM 
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function calculates the mean of the special "vector" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ## check if it already exists or needs to be calculated
        
         m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        
        x$setInverse(m)
        m
}
