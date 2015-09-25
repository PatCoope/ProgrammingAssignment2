
## Write a short comment describing this function 
#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a 
#function to 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
  invhold <- NULL
  set <- function(y) {
    x <<- y
    invhold <<- NULL
  }
  get <- function() x
  setinv <- function(inv=matrix()) invhold <<- inv
  getinv <- function() invhold
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
} 


## Write a short comment describing this function 
#The following function calculates the inverse of the special "matrix' created with the
# above function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
# via the setinv function.

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