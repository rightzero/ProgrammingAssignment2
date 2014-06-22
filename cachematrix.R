## makeCacheMatrix() and cacheSolve() all together provide a way
## to cache the inverse of a given matrix, which could speed up
## program execution if the matrix inversion is frequently needed

##  Description
##  makeCacheMatrix() creates CacheMatrix which is able to hold
##  the original matrix and the inverse.
##  The input matrix is always assumed to be invertible.
##
##  Arguments
##  m The matrix that CacheMatrix should handle the inverse cache for 
##
##  Value
##  CacheMatrix
##
##  Details
##  set(x)  Replace m with the new matrix, x
##  get()   Return m to the caller
##  setinv(i) Set the inverse of m
##  getinv()  Return the inverse of m

makeCacheMatrix <- function(m = matrix()) {
  inv <<- NULL
  
  set <- function(x) {
    m <<- x
    inv <<- NULL
  }
  
  get <- function() m
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##  Description
##  cacheSolve() checks if a CacheMatrix retuned by makeCacheMatrix() already
##  has the matrix inverse cached. If a cached value is found the cached value would be
##  returned right away; otherwise the inverse would be calculated, saved, and then
##  returned
##
##  Arguments
##  x The CacheMatrix returned by makeCacheMatrix()
##
##  Value
##  The inverse of CacheMatrix x 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}
