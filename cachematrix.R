## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <-x$getinverse
       if(!is.null(inv)) {
         message("returning inverse")
         return(inv)
       }
       data<-x$get()
       inv <-solve(data)
       x$setinv(inv)
       inv
}
