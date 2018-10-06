## Theese functions create a special object that stores square invertible matrix 
## and cache the inversion of this matrix  

## the first function makeCacheMatrix creates an matrix object "x" (a list) 
## which containing a function
## to set the matrix, to get the matrix, to set the inverse and to get the inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(y){
    
    x <<- y
    Inv <<- NULL
  }
  
  get <- function()x
  
  setInv <-  function(inverse) Inv<<-inverse 
  getInv <-  function() Inv
  
  list( set = set,
        get = get,
        
        setInv = setInv,
        getInv = getInv)
  
}

## the second function cacheSolve is due to determine if the inverse 
## of the object was already calculated and return the inversed matrix of "x" from cache
## or calculate the inverse 

cacheSolve <- function(x, ...) {
  
  Inv <- x$getInv()
  
  if (!is.null(Inv)) {
    
    message ("Getting cached data")
    return(Inv)
  }
  
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  return(Inv)
  ## Return a matrix that is the inverse of 'x'
}
