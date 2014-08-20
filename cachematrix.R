## This functions provide special "matrix" datatype and calculating
## of inverse with caching feature. It is useful when we often
## calculate inverse of matrix which are not changed

## This function creates bunch of functions to provide "matrix" type with stored inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns inverse of matrix
## and provides caching of this value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}


# some tests
test1 = matrix(rnorm(9), 3, 3)
test2 = matrix(c(1,0,0,1), 2, 2)

v = makeCacheMatrix(test1)
print(cacheSolve(v))
print(cacheSolve(v))

v$set(test2)
print(cacheSolve(v))
print(cacheSolve(v))
