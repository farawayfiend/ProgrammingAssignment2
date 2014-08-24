## makeCacheMatrix takes a matrix (must have an inverse) as an input
## and stores that matrix for use by the function cacheSolve.  It also creates a 
## list of subfunctions which are used by cacheSolve.

## cacheSolve takes the matrix stored in makeCacheMatrix and checks to see if an
## inverse has already been calculated.  If the inverse has not been calculated, 
## it calculates the inverse and returns it.  If the inverse has already been 
## calculated, it retrieves the inverse that was stored rather than recalculating.

## Stores a matrix and creates a list composed of functions used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        cachedinverse <- NULL
        setmatrix <- function(y){               #not used in cacheSolvebut 
                x <<- y                         #retained so I can use cacheSolve 
                cachedinverse <<- NULL          #on a new matrix without 
        }                                       #rerunning the entire function
        getmatrix <- function() {x}
        setinverse <- function(matrixinverse) {cachedinverse <<- matrixinverse}
        getinverse <- function() {cachedinverse}
        list (setmatrix = setmatrix, getmatrix = getmatrix, 
              setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of a matrix inputted in makeCacheMatrix

cacheSolve <- function(x, ...) {
        cachedinverse <- x$getinverse()
        if(!is.null(cachedinverse)) {
                message("getting cached data")
                return(cachedinverse)
        }
        tempmatrix <- x$getmatrix()
        cachedinverse <- solve(tempmatrix, ...)
        x$setinverse(cachedinverse) 
        cachedinverse
}
