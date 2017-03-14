## #############################################################################
## ----------------------------------------------------------------------------
## --- cachematrix.R
## --- R Programming/ProgrammingAssignment2
## ----------------------------------------------------------------------------
## --- This script is used to inverse a given matrix, if inversed is not cached already.
## ---
## ----------------------------------------------------------------------------
## --- Author contact information...
## ---    - Deepak Sharma
## ----------------------------------------------------------------------------
## #############################################################################


# ##############################################################################
#  Name : makeCacheMatrix
#
#  Purpose:
#  This function sets and gets values for input matrix and its inverse matrix
#
#  Arguments:
#    x   -      square matrix (default value : NULL matrix)
#  Example:
#    mat <- matrix(1:4,2,2,byrow=TRUE)
#    makeCacheMatrix(mat) [assumption, matrix is always inversible]
# 
#  Return:
#    A list containing set/get functions for input matrix and its inverse.
# ##############################################################################
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInvMat <- function(invMat) invMat <<- invMat
    getInvMat <- function() invMat
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


# ##############################################################################
#  Name : cacheSolve
#
#  Purpose:
#  This function inverses a given matrix, if not inversed already (cached).
#
#  Arguments:
#     x   -    object/list returned from makeCacheMatrix
#  Example:
#    mat <- matrix(1:4,2,2,byrow=TRUE)
#    x <- makeCacheMatrix(mat) [assumption, matrix is always inversible]
#    cacheSolve(x)
# 
#  Return:
#    Inversed matrix
# ##############################################################################
cacheSolve <- function(x, ...) {
    invMat <- x$getInvMat()
    if(!is.null(invMat)) {
        message("Getting cached inverse matrix")
        return(invMat)
    }
    mat <- x$get()
    invMat <- solve(mat)
    x$setInvMat(invMat)
    invMat
}
