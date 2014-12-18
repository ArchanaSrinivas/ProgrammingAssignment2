## Function: makeCacheMatrix
  ## Capability: Takes in a matrix as input and and contains functions to:
    ## Set the value of the matrix and its inverse in cached variables (set, setInverse)
    ## Get (Return) the value of the matrix and its inverse from cached variables (set, setInverse)

## Function: CacheSolve
  ## Capability: Returns the inverse of the input variable passed if cached value for the variable exists,
  ## else calculates the inverse value and returns that


## makeCacheMatix takes an matrix as input and caches its value and contains functions to cache and return its inverse
makeCacheMatrix<- function(x = matrix()) {
  
  ##Initializing inv to NULL for a new matrix call
  inv <- NULL
  
  ##Function: Set
  ##Capability: Allows ability to reset the value of the matrix variable without calling the makeCacheMatrix function again
  # Additionally, reinitializes the value of the inverse to NULL
  set <- function(newmat) {
    x <<- newmat
    inv <<- NULL
  }
  
  ##Function: Set
  ##Capability: Returns the current matrix stored in the variable
  get <- function() x
  
  ##Function: setInverse
  ##Capability: Caches the value of the inverse into the cached variable inv  
  setInverse <- function(inverse) inv <<- inverse
  
  ##Function: getInverse
  ##Capability: Returns the cached inverse of the matrix associated with the variable called
  getInverse <- function() inv
  
  
  ##List of internal methods in the makeCahceMatrix function so that a calling function knows what methods are available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the cached value of the inverse if existing, else calculates and returns the inverse of the matrix 
cacheSolve <- function(x, ...) {
  #Input: x is a matrix created using the makeCacheMatrix function
  
  #Obtain the current inverse value of the matrix passed 
  #stored in the cached variable
  inv <- x$getInverse()
  
  ##If the value returned is not NULL - then return the current cached inverse matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##This part of the function is only accessed when there is no cached inverse value for the matrix passed
  
  ##Get the value of the current matrix
  data <- x$get()
  
  ##Solve to get the Inverse
  inv <- solve(data, ...)
  
  ##Store the inverse value in the cache
  x$setInverse(inv)
  
  ##print the inverse value out
  inv
}