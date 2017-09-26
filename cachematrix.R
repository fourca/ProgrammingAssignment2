#The goal of these 2 functions is to cache a matrix along with its inversion (makeCacheMatrix),
# and be able to extract it from cached memory back in your environment whenever needed.

# In order to use makeCacheMatrix properly, one should first define the matrix as a named object 
# as in my_matrix<-matrix(x,y,z).
# The result of the function should also be stored in a separate object, 
# as in my_cached_matrix<-makeCacheMatrix(my_matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<- y
        inv <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) inv <<- solve
    getinversion <- function () inv
    list (set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


# cacheSolve takes as argument the object created by makeCacheMatrix(), 
# as in my_matrix_inversion<-cacheSolve(my_cached_matrix).
# BEWARE, in this case the result of cacheSolve() is stored in a separate object, 
# this object needs to be called in order for you to see it.
# cacheSolve() first checks if the inversion is cached, if yes it returns it.
# If not it calculates it and returns.

cacheSolve <- function(x, ...) {
    inv <- x$getinversion()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
    
}
