## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a matrix object.
## using the set and get functions.
## setmatrix = the solve funtion which calculates the inverse
## getmatrix returns the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
     x <<- y
     m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  
  getmatrix <- function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The cacheSolve function uses solve to return the inverse of a matix, (x).
## It checks to see if it is already cached (getmatrix() and returns a message if it alreay is cached.
## If it is not cached it caches it.
## returns the inverse of the matrix.



cacheSolve <- function(x=matrix(), ...) {
  
  m <- x$getmatrix()
  
  if(!is.null(m)){
       message("getting cached data")
       return(m)
  }
  
  matrix <- x$get()
  
  m <- solve(matrix, ...)
  
  x$setmatrix(m)
  
  m
}
