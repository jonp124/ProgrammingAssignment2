## Put comments here that give an overall description of what your
## functions do

# This function creates a matrix object that can cache its inverse 
# and returns a list of functions to get and set a matrix, 
# and get and set the inverse of said matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Creating an empty object that will hold the inverted matrix
  I <- NULL
  
  # Setting the matrix (Once makeCacheMatrix is assigned to a variable this function can be used to set a new matrix)
  setMatrix <- function(y){
    x <<- y
    I <<- NULL
  }
  
  # This function retrieves the matrix, x
  getMatrix <- function(){x}
  
  # This function assigns an inverse matrix to J
  setInv <- function(inv){I <<- inv}
  
  # This function returns the inverted matrix, or null if the matrix doesn't exist in the cache
  getInv <- function(){I}
  
  # Outputting a list of the functions defined above
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInv=setInv, getInv=getInv)
  
}

## Write a short comment describing this function

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
