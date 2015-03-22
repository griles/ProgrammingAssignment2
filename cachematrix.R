##  These functions work together to allow you to input a matrix (or loop through a set of matrices)
##  and calculate the inverse of each matrix efficiently (using a cached version if the inversion 
##  has already been calculated)

##  makeCacheMatrix converts an inputed matrix into a list of functions that allow 
##  caching of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
      ## this function will be used with many different matrices, so it's important to clear the value of inverse
      inverse <- NULL
      ##  this function will set the x in the makeCacheMatrix environment equal to y
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      ##  get calls a function that returns x, the matrix
      get <- function() x
      ##  setinverse is a function that solves the inverse of the matrix x and stores it in "inverse"
      setinverse <- function(solve) inverse <<- solve
      ##  this function prints the inverse of the matrix x
      getinverse <- function() inverse
      ##  this is the output of makeCacheMatrix, a list of functions created within the makeCacheMatrix env.
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve gets the inverse 

cacheSolve <- function(x, ...) {
      # get the inverse from the getinverse function in the x environment
      inverse <- x$getinverse()
      ##  check to see if the inverse has already been calculated, if so returns cached inverse and exits
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      ##  if has not been calculated, stores the x matrix in a new matrix called data
      data <- x$get()
      ##  solves the matrix data and stores the inverse in a matrix called inverse
      inverse <- solve(data, ...)
      ##  caches the inverse for future use
      x$setinverse(inverse)
      ## prints the inverse
      inverse
        ## Return a matrix that is the inverse of 'x'
}
