## makecachcematrix is a function for a matrix that cache its inverse and cachesolve does the computing

makeCacheMatrix <- function(x = matrix()) {
        # makes a matrix of invertible input for cachesolve
         inv = NULL
        set = function(y) { 
                x <<- y
                inv <<- NULL
        }
        get = function() 
        # assings value to an object in a diffent enviornment than current
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
                
cacheSolve <- function(x, ...) {
        #this is output of first matrix and returns inverse of first matrix output
        inv = x$getinv()        
        if (!is.null(inv)){       
                message("getting cached data")
                return(inv)
        }       
        # if inverse is calc'd then function gets it from cache and skips computation otherwise calcs inverse below
        mat.data = x$get()
        inv = solve(mat.data, ...)       
        x$setinv(inv)
        # sets value of the inverse
        return(inv)
}                
