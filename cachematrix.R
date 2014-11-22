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

##Still not very different from the example, 
##I've added a few status messages while solving the matrix for the first time 
cacheSolve <- function(x, ...) {
        i <- x$getcm()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        message("calculating the inverse matrix of:")
        print(data)
        i <- solve(data)
        x$setcm(i)
        message("caching the result")
        i
}

##check if the cached inverse is correct by multiplicating it with the original
##output is rounded to show only 1s and 0s
checkCache  <- function(x, ...) {
        o <- x$get()
        i <- x$getcm()
        if(is.null(i)){
                stop("Inverse matrix not yet cached, run cacheSolve first")
        }
        else
        {
                elegant_proof <- round(o %*% i)
                elegant_proof
        }
}
