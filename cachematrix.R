# This function takes a matrix, and creates a matrix object that can cache an inverted matrix 
# it also returns a list of functions to get and set a matrix, 
# and get and set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Creating an empty object that will hold the inverted matrix
  I <- NULL
  
  # Setting the matrix (Once makeCacheMatrix is assigned to a variable this function can be used to set a new matrix using variable$setMatrix)
  setMatrix <- function(y){
    x <<- y
    I <<- NULL
  }
  
  # This function retrieves the matrix, x that was passed to makeCacheMatrix
  getMatrix <- function(){x}
  
  # This function assigns an inverse matrix to I
  setInv <- function(inv){I <<- inv}
  
  # This function returns the inverted matrix, or null if the matrix doesn't exist in the cache
  getInv <- function(){I}
  
  # Outputting a list of the functions defined above
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInv=setInv, getInv=getInv)
  
}

## This function either finds the inverse of a given matrix, or pulls it from the cache
## The parameter, x, is the result of makeCacheMatrix which includes our matrix, and the list of functions
cacheSolve <- function(x, ...) {
  
  #Checking our cache for an inverse and assigning it to I
  I <- x$getInv()
  
  #Checking if the inverted matrix (I) has a value (not null)
  if(!is.null(I)){
    
    #If the matrix is not null, retreive it from the cache and return it
    message("getting cached data...")
    return(I)
  }
  
  #Otherwise, retreive the matrix using getMatrix
  m <- x$getMatrix()
  
  #This is a free variable, defined in makeCacheMatrix
  I <-solve(m,...)
  
  #Using the setInv 
  x$setInv(I)
  I
  
}
