##############################################
##                                          ##
## R Programming                            ##
## https://class.coursera.org/rprog-004/    ##
##                                          ##
## Programming Assignment 2                 ##   
##                                          ##
## author: McReyar                          ##
##                                          ##
##############################################


## In this programming assignment two functions are created to calculate
## the inversed matrix. The first time the inversed matrix is requested
## cacheSolve calculates it. For each further request the cached result is
## returned to save computation time.
## 
## Example
## cm <- makeCacheMatrix()
## cm$set(matrix(c(5,7,1,9,3,8,7,1,2), nrow = 3, ncol = 3))
## cacheSolve(cm)
## cm$get() %*% cacheSolve(cm)


## Function:    makeCacheMatrix
##
## Description: creates a list which contains functions to set/get a matrix 
##              and get/set its inverse matrix
## 
## Usage:       cm <- makeCacheMatrix(m)
##
## Arguments:   m   matrix
##                  (can be set later with set-function)
makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix
    inv <- NULL
    
    # set matrix (in environment of makeCacheMatrix)
    set    <- function(y) {
                  x   <<- y
                  inv <<- NULL
              }
    
    # get matrix
    get    <- function() x
    
    # set inverse matrix (in environment of makeCacheMatrix)
    setinv <- function(inv.m) {
                  inv <<- inv.m
              }
    
    # get inverse matrix
    getinv <- function() inv
    
    # return list of functions
    list(set    = set
        ,get    = get
        ,setinv = setinv
        ,getinv = getinv
        )
}

## Function:    cacheSolve
##
## Description: calculates the inverse of a matrix
##              (uses the cached result if available)
## 
## Usage:       cacheSolve(x, ...)
##
## Arguments:   x   list of functions created with function makeCacheMatrix
##              ... further arguments passed to solve function
cacheSolve <- function(x, ...) {
    
    # try to retrieve cached inverse matrix
    inv <- x$getinv()
    
    # if successful return chached inverse matrix
    if(!is.null(inv)) {
        message("getting cached inversed matrix")
    }
    # else calculate inverse matrix
    else {
        inv <- solve(x$get(), ...)
        # cache inversed matirx
        x$setinv(inv)
    }    
    # return inversed matrix
    inv
}