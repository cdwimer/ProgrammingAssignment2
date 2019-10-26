
##The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
##The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
##The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
##in order to retrieve the inverse from the cached matrix that is stored in the makeCacheMatrix() object's environment.

## makeCacheMatrix
# The first function, makeCacheMatrix, creates a special "matrix", 
# which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve
# The following function calculates the inverse of the special "vector" created with makeCacheMatrix. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##calculate and return inverse
        data <- x$get()
        m <- solve(data, ...) 
        x$setinverse(m)
        m
}