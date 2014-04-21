##The function makeCacheMatrix creates a custom mattrix object.
##The function cacheSolve calculates the inverse of a matrix.
##If the invere of the matrix was calculted before it will return
##the cached valuo without calculating it again.

makeCacheMatrix <- function(x = matrix()) {
   inv_x <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) xInv <<-inverse
    getinverse <- function() xInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##The function cacheSolve returns the inverse of a matrix
##created with the makeCacheMatrix function.
##If the cached inverse is not empty the function will return
##the cached value. If it is not available, the function
##will compute the value, store it in the cache and the return it.

cacheSolve <- function(x, ...) {
    if (!is.null(xInv)) {
        message("Retrieve cached inverse matrix")
        return(xInv)
    } else {
        xInv <- solve(x$get())
        x$setinverse(xInv)
        return(xInv)
    }
}