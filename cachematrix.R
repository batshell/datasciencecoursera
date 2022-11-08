makeCacheMatrix <- function( m = matrix() ) {
  #initialized variable i to contain the inverse
  i <- NULL
  
  #method to set the matrix; initially m contains and matrix and inverse is NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  #getter function for the matrix
  get <- function() {
    m
  }
  
  #setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #method to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #returns the inverse of matrix x
  m <- x$getInverse()
  
  #returns matrix directly if it is cached
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  #gets the original matrix x
  data <- x$get()
  #calculates the inverse of the matrix
  m <- solve(data) %*% data
  #sets the inverse of the matrix
  x$setInverse(m)
  #returns the matrix
  m
}
