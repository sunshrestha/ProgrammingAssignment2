## makeCacheMatrix function which can creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inv value to NULL
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  ## returns the set value
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ## return a list. 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  function cacheSolve which computes the inverse of the special "matrix" created by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  ## return the inv
  inv
}
