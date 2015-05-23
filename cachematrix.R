createTestMatrix <- function() {
  
  ## The createTestMatrix function creates a consistent input matrix for testing and validation
  
  aMatrix <- matrix(c(1,2,7,4,5,6), nrow=3, ncol=2)
  print(aMatrix)
  
}  ## End of createTestMatrix function

makeCacheMatrix <- function(matrixZ) {
  
  ## Variables:
    ## matrixZ = input matrix
    ## inverted_matrixZ = the output, an inverted matrix
  
  ## Functions:
    ## makeCacheMatrix = caches a matrix and the inverse of the matrix
    ## cacheZ function caches an input matrix
    ## retrieveZ function returns the cached matrix
    ## cacheInverseZ function creates and caches the inverted matrix
    ## retrieveInverseZ function returns the cached inverted matrix
  
  if (is.matrix(matrixZ)) {    ## Check that matrixZ is a matrix
    
    ##  if matrixZ is a matrix - create the inverted matrix
    
    inverted_matrixZ <- NULL    ## initialize the inverted matrix
    
    cacheZ <- function(matrixToSet) {
      
      ## the cacheZ function caches a matrix
      ## matrixToSet = the input matrix to be cached
      
      matrixZ <<- matrixToSet
      inverted_matrixZ <<- NULL
    }
    
    retrieveZ <- function() matrixZ  ## retrieveZ returns the cached matrix
    
    cacheInverseZ<- function(inverse) inverted_matrixZ <<-inverse  ## cacheInverseZ creates and caches the inverted matrix
    
    retrieveInverseZ <- function() inverted_matrixZ  ## retrieveInverseZ returns the cached inverted matrix
    
    ## Use the LISt function to ensure that the function makeCacheMatrix returns all functions created withn the lexical scoping
    
    list(cacheZ, retrieveZ, cacheInverseZ, retrieveInverseZ)
    
  } ## End of IF (if input is a matrix)
  
  else{  ## input is NOT a matrix
    
    inverted_matrixZ <- NULL
    print("Input is not a matrix")
    
  }  ## End of of ELSE
  
  
}  ## End of makeCacheMatrix function

cacheSolve <- function(matrixZ) {
  
  ## The cacheSolve function return the inverse of the input matrix matrixZ
  
  ## Variables:
    ## matrixZ = inout matrix
  
  
  inverted_matrixZ <- retrieveInverseZ(matrixZ)  ## set inverted_matrixZ equal to the cached inverted matrix (if caches inverse exists)
  
  if (!is.null(inverted_matrixZ)) {  ## Checked to see if cached version exists (is not NULL)
    
    ## The matrix returned by function is NULL so no cached version exists
    
    inverted_matrixZ <- solve(matrixZ$retrieveZ())  ## Cache the original matrix
    matrixZ$cacheInverseZ(inverted_matrixZ)  ## Cache the inverted matrix 
    return(inverted_matrixZ)  ## Return the inverted matrix
    
  } else {  ## Cached inverted matrix exists
    return(inverted_matrixZ)
    
  }  ## End of ELSE
}  ## End of cacheSolve function