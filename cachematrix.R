## This function will be Caching the inverse of the matrix.
## This function will store a matrix,compute its inverse and caching it for future use.
## The purpose of cahcing is to use it when needed and do not compute it every time.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  
  invrs <- NULL
  
  #set the matrix
  set <- function(mtrix) {
    m <<- mtrix
    invrs <<- NULL
  }
  
  #Get the matrix
  get <- function() m
  
  
  
  #Set the inverse of matrix.
  setInverse <- function(inverse) invrs <<- inverse
  
  
  #Get the inverse of matrix.
  getInverse <- function() invrs
  
  
  #return the list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This will return the inverse of matrix that we will get through "makeCacheMatrix" function.
## If the inverse will has also been calculated then this function will get the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  invrs <- x$getInverse()
  
  #Getting Cached Data
  if (!is.null(invrs)) {
    message("Retrieving Cached Data")
    return(invrs)
  }
  
  #Getting the matrix
  mtrix <- x$get()
  
  #calculating the inverse
  invrs <- solve(mtrix, ...)
  
  #Setting the inverse
  x$setInverse(invrs)
  
  #Returing the inverse object.
  invrs
}
