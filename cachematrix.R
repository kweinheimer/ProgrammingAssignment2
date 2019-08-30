## Put comments here that give an overall description of what your
## functions do


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize the inverse property
      i <- NULL
      
      ## Method to set matrix
      set <- function(y) {
            x <<- y
            i <<- NULL
      }

      ## Method to get matrix
      get <- function() x
      
      ## Method to set inverse of matrix
      setinverse <- function(inverse) i <<- inverse
      
      ## Method to get inverse of matrix
      getinverse <- function() i
      
      ## Return a list of the methods
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix" above. 
## If inverse has already been calculated, then "cacheSolve" should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      
      ## Only return the inverse it it's already set
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
            
      }
      ## Get matrix from our object
      data <- x$get()
      
      ## Calculate the inverse through matrix multiplication
      m <- solve(data) %*% data
      
      ## Set the inverse to the object
      x$setinverse(m)
      
      ## Return the matrix
      m
}
