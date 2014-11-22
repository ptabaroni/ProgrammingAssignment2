##constructor Than will define one instance of the cacheMatrix Obj
##Same as Vector example so far
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setcm <- function(inverse) i <<- inverse
  getcm <- function() i
  list(set = set, get = get,
       setcm = setcm,
       getcm = getcm) 
  
}

##Here as well it follows the same example function
##...i feel a little dirty but as long as it works...
cacheSolve <- function(x, ...) {
  i <- x$getcm()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setcm(i)
  i
}
