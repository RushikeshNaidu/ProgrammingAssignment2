Rprogramming Assignment2 - Inverse of a Matrix

# Finding the inverse of a matrix each time would be a tideous task .
# THus it would benefit the programmer to cache the inverse of a matrix rather
# than computing it repeatedly . 

#The makcacheMatrix creates a new special matrix as defined by the user
# and can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#This cacheSolve function will compute the inverse of the
# square matrix . If the inverse is already existing it would
# cache the same value .

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  final <- x$get()
  
  #The solve function is used to compute the inverse of a square-???matrix . 
  
  i <- solve(final, ...)
  x$setInverse(i)
  i
}
