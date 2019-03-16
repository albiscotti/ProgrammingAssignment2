## cachematrix.R for submission of ProgrammingAssignment2 by albiscotti

## This function creates a special "matrix" object that caches its inverse.

## makeCacheMatrix gets a matrix as an input, 
## sets the value of the matrix,
## gets the value of the matrix, 
## sets the inverse Matrix and 
## gets the inverse Matrix. 
## The matrix object can cache its own object. 

## <<- operator is used to assign a value to an object in an environment that is different 
## from the current environment 

makeCacheMatrix <- function(x = matrix()) {             ## take matrix as input
        invMat <- NULL
        setMat <- function(y) {                         ## set the value of the matrix
                x <<- y
                invMat <<- NULL
        }
        
        getMat <- function() x                          ## get the value of the matrix
        setInv <- function(inverse) invMat <<- inverse  ## set the value of the invertible matrix
        getInv <- function() invMat                     ## get the value of the invertible matrix
        list(setMat = setMat, getMat = getMat,
             setInv = setInv, 
             getInv = getInv)
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## The function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache, skips the computation, and returns a message. 
## Otherwise, it calculates the inverse of the data using the solve function, and sets the value 
## of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {                        ## Return a matrix that is the inverse of 'x'
        
        #get the value of the invertible matrix from the makeCacheMatrix function
        invMat <- x$getInv()
        if(!is.null(invMat)) {                          ## if inverse matrix is not NULL
                message("getting cached data")          ## Type message 
                return(invMat)                          ## Return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMat()                        ## Get the original Matrix Data 
        invMat <- solve(MatrixData, ...)                ## Use solve function to inverse the matrix
        x$setInverse(invMat)                            ## Set the invertible matrix 
        invMat                                          ## Return the invertible matrix
}
