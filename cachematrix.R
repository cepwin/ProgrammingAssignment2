## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This method takes an invertable matrix, caches it and returns a list of methods
##on that matrix with the stored matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y) {
      x<<- y
      inverse<<- NULL
  }
  get<- function() x
  setinv <- function(inv) inverse <<-inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##this method takes a *special* matrix created in makeCacheMatrix
##and if the inverse is cached already returns that
##otherwise solves for the matrix inverse, caches it (in the special matrix) and the
##returns it (the inverse of the matrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <-x$getinverse
       if(!is.null(inv)) {
         message("returning inverse")
         return(inv)
       }
       data<-x$get()
       inv <-solve(data, ...)
       x$setinv(inv)
       inv
}
