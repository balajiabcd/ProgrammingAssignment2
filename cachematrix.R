
## create list of 4 fuctions (get,set) <-> (matrix,inverse)

makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y) {
            x <<- y
            inver <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inver <<- inv
      getinverse <- function() inver
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cache function

cacheSolve <- function(x, ...) {
      inver <- x$getinverse()
      if(!is.null(inver)) {
            message("getting cached data")
            return(inver)
      }
      data <- x$get()
      inver <- solve(data, ...)
      x$setinverse(inver)
      inver
}
