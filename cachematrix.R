## The functions are used to cache the inverse of a matrix

## Creates a special matrix
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #m will store inverse of the matrix sent in x
  #x is the actual matrix
  m <- NULL
  #Setting variables
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Get the matrix
  get <- function() x
  #Set the inverse by assigning the result of solve to m
  setinverse <- function(solve) m <<- solve
  #Get the inverse
  getinverse <- function() m
  #Construct a list of all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
#via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Get the inverse
  m <- x$getinverse()

  if(!is.null(m)) {
    #If inverse already exists in cache, return it
    message("getting cached data")
    return(m)
  }
  #Get the matrix
  data <- x$get()
  #Calculate the inverse
  m <- solve(data, ...)
  #Set the inverse
  x$setinverse(m)
  #Return the inverse
  m
}
