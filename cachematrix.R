## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ## initialize a matrix and its inverse
                x <<- y
                m <<- NULL
        }
        get <- function() x   ## return the matrix
        setinverse <- function(inverse) m <<- inverse   ## assign parameter "m" with the inverse of the matrix
        getinverse <- function() m    ## return the inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getinverse()      
        if(!is.null(m)) {       ## check whether the inverse of a matrix is cached, return its inverse if cached
                message("getting cached data")
                return(m)
        }
        data <- x$get()        ## calculate the inverse of matrix and cache it
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## an example
set.seed(1)   ## set up the random number seed 
x <- matrix(runif(9), ncol=3)  ## generate a 3 by 3 matrix with elements of uniformly distributed random numbers in [0,1]
y<-makeCacheMatrix(x)   ## test the above code
y$get()
y$getinverse()
cacheSolve(y)
y$getinverse()
