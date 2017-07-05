## makeCacheMatrix creates a "matrix" object that caches its inverse.
## cacheSolve retrieves and returns the cached inverse of a special"matrix" object, or calculates and returns the inverse of said object if the cache contains NULL. 
 

## returns a list containing functions to set, get, solve for the inverse of , or get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## checks to see if their is a cached inverse for the special "matrix" and returns it if their is. Otherwise calculates and returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
