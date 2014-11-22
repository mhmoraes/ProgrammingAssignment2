

## Put comments here that give an overall description of what your
## functions do
## The funcitons together will calculate the inverse of a matrix and
## store that value on cache,if the same calculation is asked they will retrieve
## the already calculated value. 

## makeCacheMatrix function will:
##set the value of the matrix
##get the value of the matrix
##calculate the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve will calculate the inverse of the matrix using solve() function and cache the result

##If the inverse is already on the cache it will returned the cached data

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
