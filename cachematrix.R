##This function creates a special "matrix" object that can cache its inverse
##
## I used the given function as model, 
## I changed mean to inverse and used the function solve() as suggested.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


  
## cacheSolve" computes the inverse of the special "matrix
## I used again the one given as example, but using the functon solve()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}  
        ## From R Console:
> a <- diag(5,3)
> a
[,1] [,2] [,3]
[1,]    5    0    0
[2,]    0    5    0
[3,]    0    0    5
> CachedMarix <- makeCacheMatrix(a)
>cacheSolve(CachedMarix)
[,1] [,2] [,3]
[1,]  0.2  0.0  0.0
[2,]  0.0  0.2  0.0
[3,]  0.0  0.0  0.2
