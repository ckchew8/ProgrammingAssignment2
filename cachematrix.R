#=========================Start of makeCacheMatrix==========================================
# makeCacheMatrix() is basically a function that stores a list of functions.
# x: a square invertible matrix (our assumption).
# makeCacheMatrix() returns: a list containing functions to:
#              i. set()   : set the matrix         
#             ii. get()   : get the matrix         
#            iii. setinv(): set the inverse matrix 
#             iv. getinv(): get the inverse matrix 
# Values stored in variables (i-iv) will be used to for cacheSolve() function computation.

makeCacheMatrix<-function(x = matrix()) 
{
  #Assign inverse with null value
        inverse <- NULL
  
  #Modifies matrix x value.
  #'<<-' without the quotes assigns y to object 'x' without quotes in an 
  #environment that is different than current environment.
  #inverse is nullified because old inverted matrix is not needed anymore.
       set <- function(y) 
        {
          x <<- y
          inverse <<- NULL
        }
  
  #Returns value of matrix x stored.
        get <- function() x
  
  #setinv() assigns the inverse matrix to object 'inverse' without quotes in an
  #environment that is different than current environment. 
  #getinv() returns the value of inverse.
  #inv is the assigned to setinv function which is the inverted matrix of x. 
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
  
  #function list for storing 4 functions 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#===========================End of makeCacheMatrix()==========================================

#================================Start of cacheSolve()========================================
#cacheSolve() function basically computes the inverse matrix of x 
#if it is not computed already, using the solve() function.
#Else if the inverse of matrix x has already been computed, cacheSolve() 
#will return the inverted matrix without any computation.
#x: Output of makeCacheMatrix() function.

cacheSolve <- function(x, ...) 
{
  #assigns inverse with inverse values (if any) 
        inverse <- x$getinv()
  
  #Verify that value of inverse is not null.
  #If TRUE, then cached data will be used which is stored in an environment 
  #that is different than current environment and return the inverted matrix
  #without any computation & end the function. 
        if(!is.null(inverse))
          {
              message("getting cached data")
              return(inverse)
          }
  
  #If inverse is null, then the inverted matrix of x is computed 
  #by obtaining the original matrix (using x$get()) and followed by
  #using the solve() function to solve for the inverse matrix.
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
#==================================End of cacheSolve()========================================