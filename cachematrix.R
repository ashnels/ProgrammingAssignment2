##Creates two function makeCacheMatrix and cacheSolve. 
#makeCacheMatrix creates a cache variable for the matrix inverse. 
#cacheSolve checks if a matrix inverse is already cached. If its cached, it returns the cached matrix inverse
#If not, it calculates the inverse of the matrix and caches the results in the cache variable.
#Function creating Matrix object
makeCacheMatrix <- function(x = matrix()) {
  #Creates a null variable m 
  m <- NULL
  #Creates function y that assigns (and updates) y value to x
  set <- function(y) {
    x <<- y
    # assigns (and updates) null value to m
    m <<- NULL
  }
  get <- function() x
  #function that takes calculated matrix inverse and caches it
  setinverse <- function(solve) m <<- solve
  #Function that retrieves cached matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#Function that checks for cached matrix inverse.
cacheSolve <- function(x, ...) {
  #Calls getinverse function and applies to matrix x
  m <- x$getinverse()
  #If the matrix inverse already is cached in m
  if(!is.null(m)) {
    #Message to user that program is getting cached data
    message("getting cached data")
    #returns cached matrix inverse
    return(m)
  }
  #Else call get function to put matrix into data
  data <- x$get()
  #Calculate inverse of new matrix (data) and put into variable m
  m <- solve(data, ...)
  #Calls setinverse function on m to cache new matrix inverse
  x$setinverse(m)
  #Returns matrix m
  m
}