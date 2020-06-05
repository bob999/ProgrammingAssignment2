## Rprogramming Coursera Assignment 2
## 
## Bob Merrill      June 2020
##
##
##  makeCacheMatrix: this function creates an object (a cache matrix)  which stores a matrix and (possibly) its inverse
##
##  cacheSolve:      this function operates on a "cache matrix object" and returns its inverse (updating the object if required)
##

##  Create a "cache matrix" object with two properties:
##              1) a  "matrix" property containing the matrix itself
##              2) an "inverse" property containing the matrix's inverse (NA if the inverse does not exist)
##
makeCacheMatrix <- function(x = matrix()) {
    #              The next few lines do error checking to ensure the matrix has an inverse.  This is not strictly necessary, because the assignment
    #              said we could assume that all matrices have inverses, but they don't and one of my test cases was accidently singular...
    detm     <- tryCatch({det(x)}, error=function(errorcondition) {NA}, warning=function(warningcondition) {NA}, finally=function() {NA}) 
    if (is.na(detm) | zapsmall(detm) == 0) {
        inverse <- NA
    } else {
        inverse <- NULL
    }
    #
    get        <- function()    x
    getinverse <- function()    inverse
    setinverse <- function(inv) inverse <<- inv 
    list(get = get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve:  This function returns the inverse of a "cache matrix" object, either from it's property or by calculating and setting the property

cacheSolve <- function(x, ...) {
    if (!is.null(x$getinverse())) {
        return(x$getinverse())
    }
    data         <- x$get()
    inverse      <- solve(data)
    x$setinverse(inverse)
    inverse
}
