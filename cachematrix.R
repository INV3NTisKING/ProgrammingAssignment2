## The following are pair of functions that create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInvert <- function(invert) i <<- invert
        getInvert <- function() i 
        list(set = set, 
             get = get, 
             setInvert = setInvert, 
             getInvert = getInvert)

}


## This function computes the inverse of the special "matrix" created by the
## "makeCacheMatrix" function. However, if the calculation has already occured, and the matrix itself has no change,
## then the inverse will be retrieved from the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getInvert()
        if (!is.null(i)) {
                message("getting cache data")
                return(i)
        }
        date <- x$get()
        i <- solve(data, ...)
        x$setInvert(i)
        i
}
        
        ## Return a matrix that is the inverse of 'x'

source("DSCourse_2_Assignment_2/makeCacheMatrix.R")
test <- makeCacheMatrix()

summary(test)
#        Length Class  Mode    
#set       1      -none- function
#get       1      -none- function
#setInvert 1      -none- function
#getInvert 1      -none- function

test$set(matrix(1:4,2,2))

test$get()
#        [,1] [,2]
#[1,]    1    3
#[2,]    2    4

cacheSolve(test)
#        [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve(test)
#getting cache data
#        [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
