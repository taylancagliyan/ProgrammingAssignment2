## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #Firstly, we assign the inv because later we will use it.
  set <- function(y){
    x <<- y   ## <<- funtion assgin the variable in another enviroment.
    inv <<- NULL
  }
  
  get <- function() {x} # after assing the matrix firstly, we call it.
  setinverse <- function(inverse) {inv <<- inverse} # then we assign the inverse of the matrix.
  getinverse <- function() {inv} # we call the inverse matrix if it is in the cache
  list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # we look a the cache inverse inverse matrix
  if ( !is.null(inv)){
    message("getting cached data") # if the inverse matrix in the cache we call it. if it is not, we make tha calculation.
    return(inv)
  } 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

#Testing the functions

mtrx <- matrix(1:4,2,2)
a <- makeCacheMatrix(mtrx)
a$getinverse()
b <- cacheSolve(a)
b
