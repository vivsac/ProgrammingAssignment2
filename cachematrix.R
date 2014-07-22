## Put comments here that give an overall description of what your
## functions do

## this function creates special object that will 
## set value of a matrix
## get value of a matrix
## set inverse of a matrix to cache
## get inverse of a matrix from cache


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## gets the matric
  get <- function() x
  ## sets the inverse in cache
  setinv <- function(solve) inv <<- solve
  ## gets the inverse from cache
  getinv <- function() inv
  ## the following returns the list that is used to access the various functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the "matrix" returned 
## by makeCacheMatrix function and sets the inverse in cache.
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache using the getinv above.

cacheSolve <- function(x, ...) {
   inv <- x$getinv()

   if(!is.null(inv)){
     message("getting cached data")
     return(inv)
   }
   
   matrix <- x$get()
   inv <- solve(matrix, ...)
   x$setinv(inv)
   inv
   ## Return a matrix that is the inverse of 'x'
}