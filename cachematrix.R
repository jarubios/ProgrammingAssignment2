## Javier Rubio

## Assingment 2 consists of setting a function that calculates the inverse of the matrix and caches it.
## For it to work it must be a square matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrixdata<-x$get()
  m<-solve(matrixdata,...)
  x$setinverse(m)
  m
}