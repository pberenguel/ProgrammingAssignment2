## I modified the original code provided by coursera to allow for matrices instead of Vectors
## and changed the function Mean from the original code to the Function Solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get, setInv = setInv,  getInv = getInv)

}


## Write a short comment describing this function
## This function returns the inversion of a Matrix.
## If the inverted matrix is already in the environment cache, returns the matrix stored in it,
## If the inverted matrix is not already calculated, calculates it, and then return it.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m                           
}


## Some Test Cases:
## Case 1) 2x2 Matrix
## z<-makeCacheMatrix()
## a<-matrix(1:4, 2, 2)
## z$set(a)
## cacheSolve(z)
## cacheSolve(z)   -- this request will give you the cached matrix

## Case 2) 3x3 Matrix
## z<-makeCacheMatrix()
## a <- matrix(c(7, 2, 1, 0,3, -1, -3, 4, -2), nrow = 3, ncol = 3, byrow = TRUE)
## z$set(a)
## cacheSolve(z)
## cacheSolve(z)   -- this request will give you the cached matrix

## Case 3) 3x3 Matrix - Singular Matrix
## z<-makeCacheMatrix()
## a<-matrix(1:9, 3, 3)
## z$set(a)
## cacheSolve(z)