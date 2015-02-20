## Asignment2: Caching the Inverse of a Matrix 
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly. The pair of functions below cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## creates a special matrix
        m <- NULL ## assigns null as value to m
        set <- function(y) { ## defines set as a function of y
                x <<- y ## assigns x the value of y
                m <<- NULL ## assigns null as value to m
        }
        get <- function() x ## assigns x as a function to get
        setsolve <- function(solve) m <<- solve ## defines the value to setsolve
        getsolve <- function() m ## assigs m as value to getsolve
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) ## creates the list of values
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() ## assigns getsolve value to m
        if(!is.null(m)) { ## if m is not null
                message("getting cached data") ## this message is printed
                return(m) ## the value of m is printed
        }
        data <- x$get() ## assigns the value of get to data
        m <- solve(data, ...) ## creates an inverted matrix and assigns to m
        x$setsolve(m) 
        m
}
