## The functions will allow us to calculate repeat inversions by caching  
## and thus provide benefits

## makeCacheMatrix will allow us to create a list containing a function to
## set value, get value, set inverse value, get inverse value

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function returns the inverse of the matrix by first checking
## if the inverse has been calcuated
##function assumes the matrix is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
