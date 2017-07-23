## MakeCacheMatrix creates a object which can be used
## to store the inverse of a matrix.
## It also resets the m parameter, which will be used by cachesolve 
## to see whether the inverse has already been cached.
## 
## Cachesolve checks whether an inverse has already been calculated
## and either returns it from the cache or calculates it
##
## Liked the other assignment better
## 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data. Happy now?")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}




