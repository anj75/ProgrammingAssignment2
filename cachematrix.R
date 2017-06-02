## There are 2 functions namely:
## 1.makeCacheMatrix() which creates a special "matrix" object that can cache its inverse
## 2. cacheSolve() which takes the matrix returned by first function and calculates
##   its inverse.If the inverse has already been calculated and the matrix hasn't
##   changed, then it retrieves the cached matrix

## makeCacheMatrix() function creates a matrix object and returns a list of 
## functions to set the matrix, get the matrix, set the inverse and
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
      invs <- NULL
      set <- function(y){
        x <<- y
        invs <<- NULL
      }
      get <- function() x
      setinverse <- function() invs <<- solve(x) ## calculate inverse of x
      getinverse <- function() invs  ## get inverse of x
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


## cacheSolve() takes the matrix created in makeCacheMatrix() and returns the
## inverse. If the inverse has been calculated then it retrieves the cached
## inverse otherwise calculates the inverse.

cacheSolve <- function(x, ...) {
      invs <- x$getinverse()
      
      ## if inverse already exists then get it from cache
      if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
      }
      
      ## else calculate the inverse
      data <- x$get()
      invs <- solve(data, ...)
      x$setinverse()
      invs
      ## Return a matrix that is the inverse of 'x'
}
