# This function checkes the catche of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
        set = function(y) {
            x <<- y
                inv <<- NULL
        }
        get = function() x
        invset = function(inverse) inv <<- inverse 
        invget = function() inv
        list(set=set, get=get, invset=invset, invget=invget)
}


# this function checks if there is any catche and computes the inverse

cacheSolve <- function(x, ...) {
inv = x$invget()
 if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
   mat.data = x$get()
        inv = solve(mat.data, ...)
      x$invset(inv)
        
        return(inv)
}
