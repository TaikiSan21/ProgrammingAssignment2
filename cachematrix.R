## These functions provide a way to calculate the inverse of
## a matrix and cache it for future use. 

## makeCacheMatrix takes a matrix as inpuit. 
## It returns a list that contains four functions:
## Set: a function that sets the cached value of a matrix and resets the inverse to null
## It is required so that we can calculate inverses for new matrices
## Get: a function that returns the matrix we want the inverse of
## SetInv: a function that sets the value of the inverse as a cached value
## GetInv: a function that gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      ## First initialize value of inverse as null
      inv <- NULL
      
      ## Sets the cached value of x to new matrix y and reset inv
      ## inv must be set to NULL again so we know matrix has changed
      Set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## Get the matrix
      Get <- function() x
      
      ## Set the cached inverse to new value
      SetInv <- function(inverse) inv <<- inverse
      
      ## Get the value of inv
      GetInv <- function() inv
      
      ## Return the list of the four functions
      list(Set=Set, Get=Get, SetInv=SetInv, GetInv=GetInv)
}


## This function takes the list returned by makeCacheMatrix as an input,
## and returns the inverse of that matrix using solve(). First we check
## if the inverse is already cached, then if it is not we solve for it.

cacheSolve <- function(x, ...) {
      ## First we will get the value of GetInv from x 
      inv <- x$GetInv()
      
      ## If it is not null, we have already calculated the inverse so we do nothing
      if(!is.null(inv)) {
            print("Getting cached inverse:")
            return(inv)
      }
      ## If it is null, we calculate the inverse using solve and cache it
      else {
            ## First get the matrix, then solve for inverse and cache it
            matrix <- x$Get()
            inv <- solve(matrix)
            x$SetInv(inv)
      }
      ## Return the inverse
      inv
}
