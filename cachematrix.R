## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function transforms a matrix to an object having functions which enable the setting and retrieval of 
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    #This function sets the environment variable x to the matrix so that it can be later retrieved
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    #This function retrieves the stored value of the matrix 
    get <- function() x
    #This function sets the environment variable inv_x to the provided inverse so that it can be later retrieved
    setinv <- function(inver) inv_x <<- inver
    #This function retrieves the stored inverse
    getinv <- function() inv_x 
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function uses the object obtained by applying makeCacheMatrix to a matrix to calculate the inverse 
## of the matrix if a cached copy is not available and to then put it into the cache. If a cached copy is 
## available, it returns that value immediately

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinv()
    # Check if we are getting back a cached copy. If yes, return that value
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    # Calculate the inverse if a cached copy is not available and store it in the cache
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinv(inv_x)
    inv_x
}
