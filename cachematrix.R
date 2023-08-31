#Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL 
  
  #Set the values of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get the values of the matrix
  get <- function() x 
  
  #Set the values of the inverse
  setinverse <- function(inverse) m <<- inverse 
  
  #Get the values of the inverse
  getinverse <- function() m 
  
  #Return a list of all of the functions 
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
}

#Create a function that calculates the inverse, if it has already been calculated, it will find the inverse in the cache
cacheSolve <- function(x, ...) {
  
  #Look for the inverse in the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Get the values of the matrix
  data <- x$get()
  
  #Solve the matrix to get the inverse
  m <- solve(data, ...)
  
  #Set the inverse in the cache
  x$setinverse(m)
  
  #Return the inverse matrix
  m
}

#Test the functions
my_matrix <- matrix(1:4,2,2)
my_cache_matrix <- makeCacheMatrix(my_matrix)
my_cache_matrix$get()
my_cache_matrix$getinverse()
cacheSolve(my_cache_matrix)
