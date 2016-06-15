#makeCacheMatrix receives a matrix and returns its
#inverse.
#set -> Sets matrix value;
#get -> Returns matrix.
#setInv -> Sets inverse of matrix;
#getInv -> Returns inverse;
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                        
  set <- function(y) {                             
    x <<- y
    m <<- NULL
  }
  get <- function() x                             
  setInv <- function(inv) m <<- solve(inv)      
  getInv <- function() m                           
  list(set = set, get = get,                       
       setInv = setInv,
       getInv = getInv)
}

## Cachesolve returns the inverse of the matri and stores in 
# m.
cacheSolve <- function(x, ...) {
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
