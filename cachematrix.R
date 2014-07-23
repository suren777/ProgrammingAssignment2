## cachematrix.R is set of two functions to evaluaate a matrix inverse
## function 'makeCacheMatrix' takes a matrix as an argument and creates
## an object like output, which stores the matrix itself, its inverse
## and four methods for operating with this object;
## function 'cacheSolve' first performs check if inverse of the matrix defined in the previous function
## is already exists, if not, it evaluates an inverse


## makeCacheMatrix take as input matrix x, and outputs a list of methods

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL            ## set a NULL value for an inverse matrix
    set <- function(y) {    ## method to update components of this object
        x <<- y             ## update x
        xInv <<- NULL       ## update inverse of x
    }
    get <- function() x     ## method to get a data from an object
    setinv <- function(inv) xInv <<- inv ## method to update an inverse value of x
    getinv <- function() xInv            ## method to retrieve an inverse value of x from cache 
    list(set = set, get = get,           ## output
         setinv = setinv,
         getinv = getinv)
}
 



## function 'cacheSolve' takes as input an object generated with above function
## and outputs inverse of matrix, stored in this object

cacheSolve <- function(x, ...) {
        
    xInv <- x$getinv() ## check if inverse is already evaluated
    if(!is.null(xInv)) {
        message("getting cached data")  ## if yes, print the message and return the matrix inverse
        return(xInv)
    }
    matrix <- x$get() ## if not, get the value of the matrix from the object x
    xInv <- solve(matrix, ...) ## evaluate the inverse
    x$setinv(xInv) ## store inverse inside the object
    xInv ## return the inverse
    
    
}

