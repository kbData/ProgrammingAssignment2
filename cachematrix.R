## the function returns a special matrix object
## which can be passed as an argument to the function
## cacheSolve for cache inverse matrix computation
makeCacheMatrix <- function(x = matrix()) {
  cachedInverseMatrix <- NULL #initialize cache with NULL (empty cache)
  
  #the function set is used change the value of our
  #special matrix object
  set <- function(y) {
    x <<- y
    
    #we need to reset the cache here
    #because it is invalid if the value
    #of the matrix is changed.
    cachedInverseMatrix <<- NULL
  }
  
  #the function get is used to get
  #the underlying real matrix of our special matrix
  get <- function() x
  
  #this function is used internally to save the
  #computed inverse matrix into the cache
  setCache <- function(value) cachedInverseMatrix <<- value
  
  #this function is used internally to get the
  #computed inverse matrix from cache.
  getCache <- function() cachedInverseMatrix
  
  #return the special object,
  #which is just a list of 4 functions
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## this function takes a special matrix
## created by makeCacheMatrix as an argument.
## It computes the inverse matrix and
## caches the result. By repeated calls
## with the same argument the cache will be used.

cacheSolve <- function(x, ...) {
  
  ## try to get the cached inverse matrix
  cachedResult <- x$getCache()
  
  #if we have valid cache -> return its value
  if(!is.null(cachedResult)) {
    message("getting cached data")
    return(cachedResult)
  }
  
  #there was no valid cache, so we have to
  #compute the inverse matrix here
  message("calculating the inverse matrix")
  
  #we get the underlying matrix stored in
  #our special object
  data <- x$get()
  
  #the actual computation
  computedInverseMatrix <- solve(data, ...)
  
  #save result to cache
  x$setCache(computedInverseMatrix)
  
  #return the value
  computedInverseMatrix
}