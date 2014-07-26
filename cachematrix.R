# A function to create a special 'matrix'
# The function actually creats a list of functions
# the lists of functions are as follows
#  set - sets the value of the matrix
#  get - gets the value of the matrix
#  setinverse - sets the value of the inverse of the matrix
#  getinverse - gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # initialise the inverse to be NULL
        inv <- NULL
        
        #  set function - sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #  get function - gets the value of the matrix
        get <- function() x
        
        #  setinverse function - sets the value of the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        #  getinverse function - gets the value of the inverse of the matrix        
        getinverse <- function() inv
        
        
        # returns the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



# THE cacheSolve function calculates the inverse of the special 'matrix' created with the makeCacheMatrix. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        
        # Get the inverse of the special cahcedMatrix x
        inverse <- x$getinverse()
        
        # Test if inverse is null
        # If it's not null then retrive and return the cached inverse
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        
        # When the inverse is null
        # Retrieve the matrix from the cacheMatrix
        # And calculate it's inverse
        data <- x$get()
        inverse <- solve(data, ...)
        
        # Set the inverse on the cahceMatrix
        x$setinverse(inverse)
        
        # Return the calclated inverse
        inverse
        
}