## Following two funtion will demonstrate cashing inverse of
## a matrix

## The following function will cache inverse of a matrix

makeCacheMatrix <- function(x = mat()) {
        ## Initialize variables
        inv <- NULL
        
        ## Setting matrix
        set <- function( m ) {
                mat <<- m
                inv <<- NULL
        }
        
        ## Retrieve matrix
        get <- function() mat
        
        ## Set inverse matrix
        setInverse <- function(i) inv <<- i
        
        ## Retrieve inverse matrix
        getInverse <- function() inv
        
        ## Return the list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Following function is to resolve inverse of a matrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        
        ## Return the matrix m, if it is already inversed
        if( !is.null(mat) ) {
                message("getting cached data")
                return(mat)
        }
        
        ## Original Matrix (not inversed)
        data <- x$get()
        
        ## Find inverse
        mat <- solve(data) %*% data
        
        ## Set the inverse of mat
        x$setInverse(mat)
        
        ## Return 
        mat        
}
