## functions to compute and cache the inverse of matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    
    set <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x
    
    setInvMatrix <- function(inv_val){
        invMatrix <<- inv_val
    }

    getInvMatrix <- function() invMatrix
    
    list(set = set, get = get,
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
    
    invMatrix <- x$getInvMatrix()
    
    if(!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    
    myMatrixData <- x$get()
    invMatrix <- solve(myMatrixData, ...)
    x$setInvMatrix(invMatrix)   ## cache the inverse of the matrice
    
    invMatrix
}
