## makeCacheMatrix has 4 functions into it: set assign the matrix into an environment
## different from the current environment; get gets the matrix from the argument as
## a function class; setInv set the inverse from other environment to the current; getInv
## get the inverse from the other environment to the current. The output is a list with
## the values of the matrix from each function.

## cacheSolve gets the values from makeCacheMatrix and compute the inverse of the matrix
## available to read. It verify that the matrix doesn't has NA values and, in this case,
## it uses the solve() function to get the inverse from the matrix. 

##The makeCacheMatrix function makes a matrix available to interact with, getting
## a matrix object that cache the inverse of the matrix I set.
makeCacheMatrix <- function(x = matrix()){
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setInv <- function(inversa) z <<- inversa
  getInv <- function() z
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve function solve for the matrix I got from the makeCacheMatrix function,
## so I get the inverse of the matrix.

cacheSolve <- function(x,...){
  z <- x$getInv()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  matriz <- x$get()
  z <- solve(matriz, ...)
  x$setInv(z)
  z
}