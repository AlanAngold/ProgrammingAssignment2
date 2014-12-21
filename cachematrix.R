## -------------------------------------------------------------------------------
## This package helps minimize the runtime costs of using the inverse of a matrix
## by storing an invertable matrix and its inverse.  The matrix is cache'd using
## the makeCacheMatrix() routine.  The inverse matrix has to be calculated explicitly
## using the cacheSolve() routine.
## -------------------------------------------------------------------------------
## Dec 21,2014 Alan Angold

## -------------------------------------------------------------------------------
## makeCacheMatrix - Store an invertable matrix and eventually it's inverse
##                   to minimize the number of times we have perform the
##                   runtime costly inversion process.
makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix<-NULL
  # Set function for original matrix
  set <- function(givenMatrix) {
    x <<- givenMatrix
    InverseMatrix <<- NULL
  }
  # Get function for original matrix
  get <- function() x
  
  # Set the inverse matrix to local scope
  setInverse <- function(inverse) InverseMatrix <<- inverse
  # Get the inverse matrix from local scope
  getInverse <- function() InverseMatrix
  
  # Return the function object that performs the given task
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## -------------------------------------------------------------------------------
## cacheSolve - This routine will take the stored matrix and invert it, then
##              save the result.  Afterwards the inverted matrix can be returned
##              without any additional inversions.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Check to see if the inverse is already calculated.
  InvMatrix <- x$getInverse()
  if(is.null(InvMatrix)) {
    # Check to see if we've set the original matrix yet (eg. x<-makeCacheMatrix())
    Matrix <- x$get()
    if(is.na(Matrix[1,1])){
      # Nope, let the user know.
      message("ERROR(cacheSolve): Original matrix not set yet (is NA)")
    }else{
      # Original matrix present now calculate and save the inverse.
      message("Calculating inverse")
      InvMatrix <- solve(Matrix)
      x$setInverse(InvMatrix)
    }
  }
  # Return the inverted matrix.
  InvMatrix
}
