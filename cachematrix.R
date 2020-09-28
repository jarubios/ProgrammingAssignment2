## Javier Rubio

## Assingment 2 consists of setting a function that calculates the inverse of the matrix and caches it.
## For it to work it must be a square matrix

## The arguments of the function are defined, x which is a square matrix and m which is initially set to null

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
 
## Assign the input argument to X in the parent environment and null to the m object to clear any a priori values
    
     set<-function(y){
    x<<-y
    m<<-NULL
  }
 
## The input argument of the value of m is assigned in the parent environment
     
      get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  
## The list of items are named
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Function cachesolve

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