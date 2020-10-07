## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix takes one argument that is initialized as an empty matrix,
## then it makes a set of functions and returns them as a list to the parent 
## environemnt

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve retrieves cached inversed matrix from the argument, created by
## makeCacheMatrix function (x$getinv()) If there is no cached data, then 
##the function takes the original matrix from the argument (x$get()), calculates
## its inverse and saves with the x$setinv() function.

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

