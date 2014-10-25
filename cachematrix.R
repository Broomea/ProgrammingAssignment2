## The following is a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" which will:
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse
##   4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #  Initialise the local variable i that will store the inverse
  i <- NULL
  
  #  "Set" will copy the value of its parameter to the variable x and
  #  initialise the supervariable i.
  set <- function(y) {
     x <<- y
     i <<- NULL
  }
  
  #  "Get" will return the value of x in the current environment   
  get <- function() x

  #  "Setinverse" will take its parameter, globalinv, and superassign it to
  #  the supervariable i. 
  setinverse <- function(globalinv)  i <<- globalinv

  #  "Getmean" is similar to "get" but it returns the value of i, the supervariable 
  #  which contains the cached inverse if the inverse has already been calculated. 
  getinverse <- function() i
  
  #  The list statement allows us to call the "listed" functions individually 
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

##  This function calculates the inverse of the special matrix created in function makeCacheMatrix above. 
##  It checks to see if the inverse has already been calculated. If so, it retrieves the inverse from the cache and
##  skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
##  the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  #  Call makeCacheMatrix to get the value of supervariable i. 
  i <- x$getinverse()
  
  #  If supervariable i is populated (not NULL), then the inverse has already been calculated so the cached value  
  #  is returned.  The function exits at this point.
  if(!is.null(i)) {
     message("getting cached data")
     return(i)
  }
  
  #  If we get to this point, then supervariable i has not been defined (otherwise we
  #  would have exited with the return).  
  
  #  Call makeCacheMatrix to get the original value (x) and assign that to the variable data.
  data <- x$get()
  
  #  The variable "data" now has the numeric vector, so we can use the "solve" function to 
  #  calculate the inverse and put it in the local version of variable i that only exists   
  #  while this function is running.  
  i <- solve(data)
  
  #  Call the function "setinverse" to save the value of the local variable i to the 
  #  supervariable i where the value persists in cache beyond the execution of this function.   
  x$setinverse(i)
  
  #  Return the value of the inverse.
  i
}