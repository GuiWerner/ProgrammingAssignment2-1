## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        set <- function(y) {                    ## creates the set function to store the matrix passed as 'y'into cache
                x <<- y                         ## stores the matrix into cache through the 'x' variable
                m <<- NULL                      ## initializes variable m as NULL to cacheSolve function
        }
        get <- function() x                     ## creates the get function to get or set (return) a matrix
        setmatrix <- function(mtx) m <<- mtx    ## creates a function to set the variable 'm' in cache to the value of 'mtx'
        getmatrix <- function() m               ## creates a function to get the value of 'm' - if it's NULL or not
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()                      ## gets the value of 'x' in cache and set it to 'm'
        if(!is.null(m)) {                       ## checks whether 'm' is NOT NULL
                message("getting cached data")
                return(m)                       ## retrieves variable 'm' value with message "getting cached data"
        }
        initialmatrix <- x$get()                ## uses the get function to assign the initial matrix to variable 'initialmatrix'
        invertmatrix <- solve(initialmatrix)    ## calculates the inverted matrix through solve function and stores it on 'invertmatrix'
        x$setmatrix(invertmatrix)               ## uses setmatrix function to store the value of the inverted matrix into 'm'
        invertmatrix                            ## returns the inverted matrix
        
}
