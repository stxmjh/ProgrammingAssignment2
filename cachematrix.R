#R Programming Week 3 Assignment - Lexical Scoping
#=======================================================================================================
## Two functions which set and solve the inverse of a matrix, reproducing the results from cached memory once solve.
##More information on what the inverse of a matrix is and how it is calcultated can be found at: https://www.mathsisfun.com/algebra/matrix-inverse.html
#=======================================================================================================
## makeCacheMatrix.R 
#(1)Like the makeVector function, the first nested function will 'set' the objects in case the second function has already been run. 
#(2)The second getMatrix then calls the matrix object x. 
#(3)The third function sets the object gI as the inverse of the matrix 
#(4)Finally the getInverse function calls the inverse matrix. 
#=======================================================================================================
makeCacheMatrix <- function(x = matrix()) {
  objInverse    <- NULL
  
  setMatrix     <-function(y) {
    x <<- y
    objInverse <<- NULL
  }
  getMatrix     <- function() x
  setInverse    <- function(inverse) objInverse <<- inverse
  getInverse    <- function()objInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
  
}
#======================================================================================================
## cacheSolve.R 
# The function 'cacheSolve' uses the previous makeCacheMatrix fucntion and returns the inverse of 
# a matrix which, if already computed and has not changed, will be retreived from cache memory to 
# reduce computational expense and time. 
#======================================================================================================
cacheSolve <- function(x, ...){
  objInverse <- x$getInverse()
  
  if (!is.null(objInverse)) {
    message("Retrieving cached data...please wait.")
    return(objInverse)
  }
  data        <- x$getMatrix()
  objInverse  <- solve(data, ...)
  x$setInverse(objInverse)
  objInverse
}
#=====================================================================================================
#Author: MJHardcastle - December 2020