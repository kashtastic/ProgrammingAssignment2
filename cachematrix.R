# makecachematrix creates a "special" matrix object that caches its inverse
# this object is a list which: 
    # 1. sets and gets the value of the matrix
    # 2. sets and gets the value of the inverse using solve
# im for "inverse matrix"

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    set <- function(y) {
      x <<- y
      # "sets" the value of the matrix
      
      im <<- NULL
      # clear the cache
    }
    get <- function() x
    # "gets" the value of the matrix
    setim <- function(solve) im <<- solve
    # "sets" the inverse of the matrix
    
    getim <- function() im
    
    # return a list of set, get, setim, getim
    list(set = set, get = get,
         setim = setim,
         getim = getim)
}


# cachesolve returns the inverse of the special "matrix" created by makecachematrix
# cachesolve checks if the inverse has already been calculated
    # 1. if so, it "gets" the value from the cache
    # 2. if not, it calculates the inverse and "sets" the value of the inverse in cache via setinverse
# im for "inverse matrix"

cacheSolve <- function(x, ...) {
  im <- x$getim()
  # "gets" the cached value
  
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
    ## 1. from above
    
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setim(im)
  im
  ## 2. from above: cache is empty, calculate inverse, cache inverse, return cache
}
