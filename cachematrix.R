## Set the input matrix as x
## Solved value "p" as a NULL

##This function creates a special "matrix" object 
##That can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  p<-NULL
  set<-function(y){
    x<<- y
    p<<- NULL
  }
  get <- function() x
  setinverse<-function(solve) p<<-solve
  getinverse<-function() p
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


##cacheSolve is a function 
##It Computes the inverse of the special "matrix"
##If the inverse has already been calculated(and the matrix has not changed),
##Then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p <- x$getinverse()
  if(!is.null(p)) {
    message("getting inversed matrix")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}