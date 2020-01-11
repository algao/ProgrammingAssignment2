# Functions to create a Matrix Object with cached inverse

# Creates Cache Matrix object with methods to set, get value and set get inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Function that takes in a Cache Matrix object and prints the Inverse matrix if already set; 
# else solve for Inverse Matrix and set it
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Inverse Matrix found:")
    return(m)
  }
  message("Inverting matrix...")
  data <- x$get()
  m <- t(data, ...)
  x$setinverse(m)
  m
}

