## Following are the two functions to cache the inverse of a Matrix
##makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##Which is really a list containing a function to :
##1)set the value of the matrix (set)
##2)get the value of the matrix (get)
##3)set the value of the inverse (setmatrix)
##4)get the value of the inverse (getmatrix)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                               ##Intially assigning 'NULL' to m
  
  set <- function(y) {
                                          
             x <<- y                           ##setting the matrix 'x' 
             m <<- NULL                         
    }     
  get <- function()x                        ##Returning matrix 'x'
  setmatrix <- function(solve) m <<- solve  ##Cache the value of the inverse
  getmatrix <- function() m                 ##returning the inverse
  list(set = set, get = get, 
      setmatrix = setmatrix,
      getmatrix = getmatrix)
  

}


## ##Following function cacheSolve: computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
##If the inverse has already been calculated  
## then the cachesolve  retrieves the inverse from the cache.Otherwise,it calculates
## the inverse and sets the value of the inverse in the cache via the 'setmatrix' function.


cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()                      ##Getting inverse
  if(!is.null(m)){                       ##checking for the presence of inverse
    message("getting cached data")     ##If so,displays the message
    return(m)
    
  }
  matrix <- x$get()                    ##Getting Matrix
  m <- solve(matrix, ...)              ##using solve() to compute inverse
  x$setmatrix(m)                       ##To cache the inverse    
  m                                    ##Returning the inverse     
       
}
