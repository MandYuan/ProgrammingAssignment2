## The below functions cache the inverse of a matrix.
## This funciton creates a matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x   ##get the value of the Matrix
  setInverse<-function(solveMatrix) inv<<-solveMatrix ##set the inverse of the matrix
  getInverse<-function() inv   ##get the value of the invertible matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function generates the inverse of the "matrix" returned by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv) } ##retrun the invertible matrix
  data <-x$get() ## the origianl matrix data
  inv<-solve(data) ##use Solve function to inverse the matrix
  x$setInverse(inv)
  inv ##return the invertible matrix
}
