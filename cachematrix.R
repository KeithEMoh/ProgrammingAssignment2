## Function "makeCacheMatrix" returns a list of length 4 which contains functions to:
## 1. Set the value of the matrix (function "set")
## 2. Get the value of the matrix (function "get")
## 3. Set the value of the matrix inverse (function "setInv")
## 4. Get the value of the matrix inverse (function "getInv")

makeCacheMatrix <- function(x = matrix()) {
    matrixOrig <- NULL
    
    set    <- function(y) {           
        matrixOrig <<- y               ## Save the matrix in the parent environment         
        matrixInv <<- NULL             ## and initialize the matrix inverse to NULL
    }                                  ## to indicate no inverse has been computed
    
    get    <- function() matrixOrig

    setInv <- function(inv) matrixInv <<- inv

    getInv <- function() matrixInv

    list(set    = set,                 ## Create a list containing the 4 functions
         get    = get,                 ## and return the list to the caller
         setInv = setInv,
         getInv = getInv)
}


## This function calculates the inverse of the matrix associated with the above
## four functions.
## Function parameters:
## "x"   is the list of functions created by the above "makeCacheMatrix" function.
## "mat" is an OPTIONAL parameter which if specified, should be the current matrix.
##       The reason for this parameter is to satisfy the assignment requirement
##       that a check be made as to whether the original matrix has changed.
##       To check the original matrix contents with the current matrix contents,
##       the current matrix needs to be given to "cacheSolve".
##       The assignment function specification has "..." in the parameter
##       list, which means to me that additional parameter(s) can be specified.
##       If "mat" is not specified by the user, the default value of "mat" is NULL
##       and "cacheSolve" behaves like the example that caches the mean of a vector.

## Assumption as per the assignment specification is that the matrix is invertible.
## Also, it is assumed that the matrix is square and is not NULL.

cacheSolve <- function(x, mat=NULL, ...) {
    if(!is.null(mat)) {               ## If a matrix was passed as the second parm, then
        if(!identical(mat, x$get()))  ## if "mat" not equal to matrix saved in parent
            x$set(mat)                ## environment, use "set" to save "mat" in parent
                                      ## environment as the new original matrix, and
    }                                 ## set its inverse to NULL.
    m <- x$getInv()
    if (is.null(m)) {                 ## If matrix inverse does not exist,
        m <- solve(x$get())           ## compute the inverse and save it in
        x$setInv(m)                   ## the parent environment.
    }                                 
    m                                 ## Return the inverse of the matrix
}
