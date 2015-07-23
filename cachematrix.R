## R Programming Assignment 2
## Justin Elszasz

## The two functions makeCacheMatrix(x) and cacheSolve(x) increase the 
## efficiency of reusing the inverse of a matrix by caching the solution for future use.

## makeCacheMatrix takes in an invertible (square) matrix and builds a list
## object containing four functions:
## makeCacheMatrix(x)$get - returns original matrix
## makeCacheMatrix(x)$set - stores original matrix in cache
## makeCacheMatrix(x)$getinverse - returns cached inverted matrix solution if present
## makeCacheMatrix(x)$setinverse - stores inverted matrix solution in cache

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## cache the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## return the matrix
        get <- function() x
        
        ## takes the inverse that is passed and caches it
        setinverse <- function(inverse) inv <<- inverse
        
        ## returns the inverse
        getinverse <- function() inv
        
        ## return the list of 4 functions, forming the cache object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve either grabs the already-solved inverted matrix out of the cache
## and returns it, or, if it has not yet been inverted, calculates the inverse
## of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        ## grabs whatever is in the cache object inverse placeholder
        inv <- x$getinverse()
        
        ## if the value is already there then just return it.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise we need to calculate, so grab the data from the 
        ## cache object
        data <- x$get()
        
        ## calculate the inverse of the matrix
        inv <- solve(data, ...)
        
        ## call the setinv function of x which sets the inverse placeholder
        ## in the cace to the value of 'inv' 
        x$setinverse(inv)
        
        ## return the inverse value
        inv
}



