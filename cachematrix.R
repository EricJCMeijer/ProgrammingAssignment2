## This function creates a matrix, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(mat) {
      x <<- mat
      inv <<- NULL
    }
    getmatrix <- function() {
      x
    }
    setinverse <- function(inverse) {
      inv <<- inverse
    }
    getinverse <- function() {
      inv
    }
    list(set = setmatrix, get = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a matrix, 
## if not already stored in the cache
## and stores the result in the cache
## if value of the inverse matrix is in the cache
## then it uses the cached value
## it assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached inverse matrix")
      inv
    } else {
      matrix <- x$get()
      inv <- solve(matrix)
      x$setinverse(inv)
      inv
    }
}
