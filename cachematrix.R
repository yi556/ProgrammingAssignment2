#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the inverse of the matrix
  set <- function(y) {
    x <<- y     # Assign the matrix 'y' to 'x'
    inv <<- NULL  # Reset the inverse since the matrix has changed
  }
  get <- function() x  # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Store the inverse
  getInverse <- function() inv  # Return the stored inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)  # Return the list of functions
}


#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if it exists
  if(!is.null(inv)) {  # If there's a cached inverse, return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # Otherwise, get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse for future use
  inv  # Return the computed inverse
}

##Assignment completed
