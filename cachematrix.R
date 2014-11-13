


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {##something new
        
        inv <- NULL
        
        #set the original matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        #get the original matrix
        
        get <- function() x
        
        #sets variable m to the inverse of the original matrix if it has already been computed (cache)
        
        setinv <- function(solve) inv <<- solve
        
        #get the inverse of the original matrix
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        #calls the makeVector function to see if the inverse has been calculated yet.
        inv <- x$getinv()
        
        #checks if variable inv(from makeCacheMatrix func) is empty, if not empty prints the message below
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #gets the components from makeCacheMatrix func and stores it in var data
        data <- x$get()
        
        ## calculates the inv and stores it in inv
        inv <- solve(data, ...)
        
        ## populates the setinv var from makeCacheMatrix func with the invers computed above
        x$setinv(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
