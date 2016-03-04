## This file contains 2 functions:
# makeCacheMatrix - creates a holding (cached) matrix object that can calculate its inverse
# cacheSolveMatrix - retrieve the cached matrix inverse values
#
# Modification history
# ====================
# N. Hamilton v0.1 03-Mar-2016 - initial version
#
## To test/execute these functions:
# 1. create a dummy input 2 x 2 matrix with values 1 through 4
# > test_mat <- matrix(1:4, nrow = 2, ncol = 2)
# 2. call makeCacheMatrix passing test_mat as a parameter, assigning the result to special_mat. This will create the 
# cached version of the inverse
# > special_mat <- makeCacheMatrix(test_mat)
# 3. call cacheSolve passing special_mat and the cached version should be returned.
# > cacheSolve(special_mat)

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialise an empty variable
    m <- NULL
    
    # Define a function set_matrix that accepts as input a variable y and will assign the value of y to x and NULL to m in the parent environment
    set_matrix <- function (y) {
        # Superassignment of variables x and m (using <<-) to make these accessible outside the scope of the function
        x <<- y
        m <<- NULL
    }
    
    # Declare the function get_matrix which returns the value x
    get_matrix <- function() {x}
    
    # Declare the function setinverse which appys the solve (inverse) function to the global variable m (inverse matrix)
    setinverse <- function(solve) { m <<- solve }
    
    # Retrieve the inverse matrix m
    getinverse <- function() {m}
    
    #return a list of the defined functions
    list (set_matrix = set_matrix,
          get_matrix = get_matrix,
          setinverse = setinverse,
          getinverse = getinverse)

}


## Function to return a cached inverse matrix if it exists. If it does not exist, then it will be created.

cacheSolve <- function(x) {
    m <- x$getinverse
    
    # Check if the result returned is not null
    if(!is.null(m)) {
        message("Getting cached matrix")
        m
    }
    
    data <- x$get_matrix()
    # Apply the inverse function
    m <- solve(data)
    
    x$setinverse(m)
    
    ## Return a matrix m that is the inverse of 'x'
    m
}

