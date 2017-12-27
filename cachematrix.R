##This function creates a special "matrix" object that can cache its inverse - it's rather a list of functions
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y)
  {x <<- y
  inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv 
  
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set.inv(inv)
  inv
  
}
