## These functions use the cache to store data on a matrix and its inverse.

## This function makes a matrix that can be put in a cache. To call the
## function and get a cached matrix, you want to set it to a value:
## y<-makeCacheMatrix()
## This will make a variable y that is a list of 4 functions

makeCacheMatrix <- function(x = matrix())
{
  ix <- NULL
  set <- function(y) #Sets a cached matrix, storing it in the cache
  {
    x <<- y
    ix <<- NULL
  }
  get <- function() x  ## retrieves and outputs the cached matrix
  
  setinverse <- function(solve) ix <<- solve ## Sets and inverse matrix to be stored
  
  getinverse <- function() ix ## Retrieves and outputs the inverse matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## Outputting the list of functions
}


## Gets the inverse if it is stored, if it is not stored then it sets and outputs
## an inverse. If the matrix stored has no inverse then the solve function will fail

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
  ix <- x $getinverse() ## Retrieving a cached inverse
  if(!is.null(ix)) ## Checking if there is actually an inverse
  {
    message("Have a cached copy")
    return(ix) ## End the function and spit out the inverse
  }
  data <- x$get() ## Pull the original matrix
  ix <- solve(data, ...) ## Create an inverse of the original matrix
  x$setinverse(ix) ## Put that inverse into the cache
  ix ## Output the inverse
}
