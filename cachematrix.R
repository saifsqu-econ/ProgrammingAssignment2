# This script caches the inverse of a matrix to avoid recalculating it

makeCacheMatrix <- function(x = matrix()) {
# Creates a special matrix object that can store its inverse
inv <- NULL

set <- function(y) {
x <<- y
inv <<- NULL
}

get <- function() x

setinverse <- function(inverse) inv <<- inverse

getinverse <- function() inv

list(set = set,
get = get,
setinverse = setinverse,
getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# Returns the cached inverse if available, otherwise computes and caches it
inv <- x$getinverse()

if (!is.null(inv)) {
message("getting cached inverse")
return(inv)
}

data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

# Create a matrix
mat <- matrix(c(2, 1, 1, 2), 2, 2)

# Create the cache matrix object
cm <- makeCacheMatrix(mat)

# First time: computes and caches the inverse
cacheSolve(cm)

# Second time: retrieves from cache
cacheSolve(cm)
