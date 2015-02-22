## This is a function to cache a matrix and if the original matrix is unchanged it will retrieve the inverse
## rather than calculate it.

## Function to make the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This returns the inverse of the (inversible) matrix

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
