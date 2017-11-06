## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix.
# 4. get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inv) Inverse <<- Inv
  getinverse <- function() Inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function returns inverse. It will check whether 
# inverse is computed already. if yes, then it will return the result otherwise
# it computes the inverse.

cacheSolve <- function(x, ...) {
  
  iMat <- x$getinverse()
  if (!is.null(iMat)){
    message("getting cached data")
    return(iMat)
  }
  data <- x$get()
  iMat <- solve(data)
  x$setinverse(iMat)
  iMat
}

# > m=makeCacheMatrix(Mat)
# > m$get()
# [,1] [,2]
# [1,]    2    2
# [2,]    3    1
# > cacheSolve(m)
# [,1] [,2]
# [1,] -0.25  0.5
# [2,]  0.75 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,] -0.25  0.5
# [2,]  0.75 -0.5
