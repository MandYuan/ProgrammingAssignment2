## The below functions cache the inverse of a matrix.
## This funciton creates a matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(solveMatrix) inv<<-solveMatrix
  getInverse<-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function generates the inverse of the "matrix" returned by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv) }
  data <-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
