
## set the function, get the function, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## If inverse of matrix is already in cache, pull from there instead of solving again

cacheSolve <- function(x, ...) {
        inv <- x$getinv()  ### get inverse of matrix, assign to inv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
