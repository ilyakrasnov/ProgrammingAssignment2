## The function makeCacheMatrix first intruduces a global 
## variable i which will cache the inverted matrix
## It then defines the getter and setter methods needed
## to set and retreive the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
   
    get <- function() x
   
    setinverse <- function(inverse) i <<- inverse
   
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first checks if an inverse matrix is
## cached and calculates the inverse matrix using the
## solve() function if no matrix is cached.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
