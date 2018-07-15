## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function takes the matrix as input and pretty much like getter and setter i.e.
# gets the value of matrix and gets the inverse of matrix and set the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<-y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x          # get the value of matrix                    
  setInverse <- function(inverse) invMatrix <<- inverse   # set the value of the  invertible matrix
  getInverse <- function() invMatrix   # get the value of invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}



## Write a short comment describing this function
# this function takes output of above function as input and checks whether 
# inverse has any value or not , in case inverse matrix is NULL , it computes the inverse
# using solve,if inverse matrix is not null it itself returns the inverse matrix without computing
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("already cached !!")   #Type message: already cached  
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               #return the invertible matrix
}