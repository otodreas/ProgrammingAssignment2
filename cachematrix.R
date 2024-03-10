## The functions makeCacheMatrix and cacheSolve involve solving a matrix passed
## as an argument to makeCacheMatrix.

## makeCacheMatrix initializes x as a matrix as the functions argument, and
## inverse is set to NULL in the makeCacheMatrix environment. Functions set,
## get, setinverse, and getinverse are defined and a list is created where each  
## of the functions are named appropriately.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list (set = set, get = get, setinverse = setinverse, getinverse = 
                      getinverse)
}

## cacheSolve essentially checks if we already have cached a solved matrix to
## inverse, and if so, simply prints the contents of the matrix along with the
## message "getting cached data". If not (if inverse = NULL) it gets the 
## inputted matrix, calculates the inverse using solve(), sets the inverse and 
## prints it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message ("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}