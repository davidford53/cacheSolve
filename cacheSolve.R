makeCacheMatrix <- function(x=matrix()) {
  mtx<-NULL
  set<- function(y) {
    x<<-y
    mtx<<- NULL
  }
  get <- function() x
  setMtxSolve <- function(solve) mtx<<- solve
  getMtxSolve <- function () mtx
  list( set = set, get = get, setMtxSolve = setMtxSolve, getMtxSolve = getMtxSolve)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  #x$setMtxSolve(x)
  mtx <- x$getMtxSolve()
  if(!is.null(mtx)){
    message("getting cached data")
    return(mtx)
  }
  data<-x$get()
  mtx<-solve(data,...)
  x$setMtxSolve(mtx)
  mtx
  
}