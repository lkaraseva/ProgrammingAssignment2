## First function "makeCacheMatrix" creates a list of functions to work with
## a given matrix(x)- sets/gets the value of the matrix and its inversion

## Second function "cacheSolve" checks if the inversed matrix has been 
## calculated already and returns the value; if not - inverses the matrix(x)

## 1 - Set/get the matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        setmx <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        getmx <- function() x
        ## Computing the inverse of a square matrix can be done with the solve f
        ## For this assn-t, assume that the matrix supplied is always invertible
        setinversedmx <- function(solve) xinv <<- solve
        getinversedmx <- function() xinv
        list(setmx = setmx, getmx = getmx,
             setinversedmx = setinversedmx,
             getinversedmx = getinversedmx)
} 

## Return a matrix that is the inverse of 'x' (first check if the inversion exists)

cacheSolve <- function(x, ...) {
        
        xinv <- x$getinversedmx()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$getmx()
        xinv <- solve(data, ...)
        x$setinversedmx(xinv)
        xinv
}

##my own test runs

##create x as a matrix and call 2 functions above (+see what is in there)

# x<-matrix(c(1,0,2,1,1,3,3,0,1,1,1,2,0,2,0,1),4,4)
# spmx<-makeCacheMatrix(x)
# spmx
# cacheSolve(spmx)
# cache<-cacheSolve(spmx)
# cache


