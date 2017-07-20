## Caching a costly computation of inversing a matrix 


## make cacheable matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv<<-solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## return cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dataMatrix <- x$get()
  inv <- solve(dataMatrix)
  x$setinv(inv)
  inv
}
