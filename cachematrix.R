## The makeCacheMatrix calculates de inverse of a matrix the user inputs
## and store the result in a list. Then, the cache Solve function recovers 
## the result of the inverse matrix and call it. 

## Assignation of the function and its arguments
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #Processsing of the data to obtain the results 
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  #Storing the resultsin a list 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#Assignation of the function
cacheSolve <- function(x, ...) {
  #Returns a matrix with the inverse of x
  m <- x$getInverse()
  #Condition to inform if it is recovering the result with the inverse matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #Processing to recovering the inverse matrix  
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
