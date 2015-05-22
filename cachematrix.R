## The functions implement caching functionality for matrix inverse calculation where unnecessary recomputations are avoided


## The function creates a vector which is a list containing functions to set and get the value and inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
     set <- function(y) {
                x <<- y
                inv <<- NULL
        }
     get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
 list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks if the inverse has already been calculated,if yes  it returns the old value else computes the new inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
