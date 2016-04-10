#The following two functions are used to cache the inverse of a matrix in order to save computation cost. 


#This function creates a list containing a function to, Set and Get values of matrix and his inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y){
    x <<- y       ##caching
    m <<- NULL    ##caching
  }
  get <- function() x
  setmatrix <- function(solve) { matinv <<- solve }
  getmatrix <- function() { matinv }
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


# This function returns the inverse of a matrix. 
# It computes the inverse and sets the value in the cache if not already computed

cacheSolve <- function(x, ...) {
  matinv <- x$getmatrix()
  if( !is.null(matinv) )
  {
    message("getting cached data")
    return(matinv)
  }
  matrix <- x$get()
  matinv <- solve(matrix, ...)
  x$setmatrix(matinv)
  matinv
}


## x = rbind(c(1, -1/8), c(-1/8, 1)) 
## m = makeCacheMatrix(x) 
## m$get() 

