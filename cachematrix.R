## -------------------------------------------------------------------------------
## This package helps minimize the runtime costs of calculating the inverse of a matrix.
## It does this by storing an invertable matrix and its inverse.  The matrix is cache'd using
## the M<-makeCacheMatrix(matrix) routine.  The inverse matrix has to be calculated explicitly
## using the matrixInverse<-cacheSolve(M) routine.
## -------------------------------------------------------------------------------
## Dec 21,2014 Alan Angold

## -------------------------------------------------------------------------------
## makeCacheMatrix - Store an invertable matrix and eventually it's inverse
##                   to minimize the number of times we have perform the
##                   runtime costly inversion process.
makeCacheMatrix <- function(x = matrix()) {
  
  # Check to make sure the user given argument is a matrix
  if(!is.matrix(x)){
    stop("ERROR(makeCaceMatrix): Argument is *not* a matrix!")
  }
  
  InverseMatrix<-NULL
  # Set function to store copy of original matrix
  set <- function(givenMatrix) {
    x <<- givenMatrix
    InverseMatrix <<- NULL
  }
  # Get function to return copy of original matrix
  get <- function() x
  
  # Set the matrix inverse explicitly to local storage
  setInverse <- function(inverse) InverseMatrix <<- inverse
  # Get the inverse matrix from local scope
  getInverse <- function() InverseMatrix
  
  # Return the matrix-object(list) that saves the matrix and it's inverse.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## -------------------------------------------------------------------------------
## cacheSolve - This routine will take the stored matrix and invert it, then
##              save the result.  Afterwards the inverted matrix can be returned
##              without any additional inverse calculations.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Check to make sure user gave us a list object (an error I just made!)
  if(!is.list(x)){
    stop("ERROR(cacheSolve): Function requires list object")
  }
  
  # Check to see if the inverse is already calculated.
  InvMatrix <- x$getInverse()
  if(is.null(InvMatrix)) {
    # Check to see if we've set the original matrix yet (eg. check for x<-makeCacheMatrix())
    Matrix <- x$get()
    if(is.na(Matrix[1,1])){
      # Nope, let the user know.
      stop("ERROR(cacheSolve): Original matrix not set yet (is NA)")
    }else{
      # Original matrix present now calculate and save the inverse. Tell user when
      # we actually do an matrix-inverse calculation.
      message("Calculating inverse")
      InvMatrix <- solve(Matrix)
      x$setInverse(InvMatrix)
    }
  }
  # Return the inverted matrix.
  InvMatrix
}

## -------------------------------------------------------------------------------
## End of file cachematrix.R
## -------------------------------------------------------------------------------

