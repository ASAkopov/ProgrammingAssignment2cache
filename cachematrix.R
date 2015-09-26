##Two lower functions provide inverse matrix computation
##and caching of result.


##makeCacheMatrix - function returns
##a list of operations on matrix:
## set() - sets matrix (replaces previous)
## get() - returns marix
## setimat() - caches inverse matrix
## getimst() - returns inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  imat <- NULL
  set <- function(y){
    x <<- y
    imat <<- NULL
  }
  get <- function() x
  setimat <- function(invmat) imat <<- invmat
  getimat <- function() imat  
  list(set = set, get = get,
       setimat = setimat, getimat = getimat)
}


##cacheSolve - function returns inverse matrix
##or its previously cached values.
cacheSolve <- function(x, ...) {
  inv <- x$getimat()
  if (!is.null(inv)) {
    message('getting cached data...')
    return(inv)
  }
  else {
    matrix <- x$get()
    inv <- solve(matrix,...)
    return(inv)
  }    
}
