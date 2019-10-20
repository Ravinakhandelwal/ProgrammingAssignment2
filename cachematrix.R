#below function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. The function contains a pair of functions which are used to create a special object that
#store a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
inv= NULL
  set=function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setInverse=function(inverse) inv<<-inverse
getInverse=function()inv
list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## the below function cacheSolve calculates the inverse of the special 'matrix' created by the above function makeCacheMatrix and
# if the inverse are already been calculated, then the matrix has not changed. Then cacheSolve function will retrieve the inverse
#of the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
 inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
