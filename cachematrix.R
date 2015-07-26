## Programming Assignment 2 - R Programming
## Data Science Specialization Track
##
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly.These pair of functionscache the inverse 
## of a matrix.
## 
## This file provides the following functions:
##  
## makeCacheMatrix: 
##  This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: 
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.
##
###################
## makeCacheMatrix:
## Creates a special matrix function for enabling faster execution of matrix inversions. 
##
## Usage example:
## x <- matrix(c(4,3, 3,2), nrow = 2, ncol = 2, byrow = TRUE)
## m <- makeCacheMatrix(x)
###################
makeCacheMatrix <- function(x = matrix()) {
  
  inv.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inv.matrix <<- inverse
  
  # Get the inverse
  getinverse <- function() inv.matrix
  
  # Encapsulate into a list
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

###################
## CacheSolve provides the ability to use, the pre-computed result is obtained
## using, makeCacheMatrix. This reduces the overall recomputation time.
## A message will be shown in the command prompt when the pre-computed
## result is returned instead.
##
## Usage example:
## x <- matrix(c(4,3, 3,2), nrow = 2, ncol = 2, byrow = TRUE)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
## print(s)
##
## s should return:
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##
###################
cacheSolve <- function(x = matrix(), ...) {
  
  # Get the current state of the inverse and check if it
  # has been computed yet
  inv.matrix <- x$getinverse()
  
  if(!is.null(inv.matrix)) {
    #Take advantage return the computed inverse		
    message("getting cached data")
    return(inv.matrix)
  }
  
  # get the stored matrix data
  matrix.data <- x$get()
  
  # find the inverse
  inv.matrix <- solve(matrix.data, ...)
  
  # Update this result in the object 
  x$setinverse(inv.matrix)
  inv.matrix <- x$getinverse()
  
  # Return this new result
  inv.matrix   
}  
