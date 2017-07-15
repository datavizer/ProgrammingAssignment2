## This code allows the inverse of a matrix using two functions 
## that provides the inverse of a matrix, which must be a square matrix 

## The first function 'makeCacheMatrix' creates a list that 
## 1.  sets the value of a matrix
## 2.  gets the value of a matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

## The function can be run on an empty value or on a matrix.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setsolve <- function(inverse) inv <<- inverse
                getsolve <- function() inv
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## The cacheSolve function returns a matrix that is the inverse 
## of the matrix that is stored in $set 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        data
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv        
        
}
