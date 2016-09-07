## Caching the Inverse of a Matrix

## makeCacheMatrix stores the "Matrix" and "InverseMatrix" (it can be null or not-null)
## It implements setters and getters for these 2.

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) InverseMatrix <<- inverse
  getinverse <- function() InverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves for inverse of a matrix
## If Inverse is already cached(available) it uses cached value instead of calculating it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getinverse()
  if(!is.null(InverseMatrix)) {
    message("getting cached inverse matrix")
    return(InverseMatrix)
  }
  Matrix <- x$get()
  InverseMatrix <- solve(Matrix)
  x$setinverse(InverseMatrix)
  InverseMatrix
}
