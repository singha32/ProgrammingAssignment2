## Put comments here that give an overall description of what your
## functions do
makeCacheMatrix <- function (x) {
  print(x)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {m <<- inverse}
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

#a <- makeCacheMatrix(matrix(c(5,2,7,6), 2, 2))

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