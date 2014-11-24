## 2 functions: the first one creates a special matrix and caches its inverse, the second function computes the inverse

## This function creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  s <- NULL
  set <- function(y) 
  {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) 
  {
    s <<- solve
  }
    
  getInverse <- function() 
  {
    s
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}
