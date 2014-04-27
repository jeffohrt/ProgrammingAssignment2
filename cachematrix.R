## storing stuff with <<- operator

## creating storage object

makeCacheMatrix <- function(x = matrix()) {
  
  ## wanted is what I am after - in this case - an inverse matrix
  
  wanted <- NULL
  
  set <- function(y){
    
    ## this is the fancy storage trick with <<-
    
    x <<- y
    
    wanted <<- NULL
  }
  
  get <- function() x
  
  ## solve is the native R function for inverting matrices
  
  setsolve <- function(solve) wanted <<- solve
  
  getsolve <- function() wanted
  
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)  

}


## inverse exists or not, return inverse

cacheSolve <- function(x, ...) {
  
  ## this grabs the stored copy if it is there
  
  wanted <- x$getsolve()
  
  ## if the inverse exists, return it
  
  if(!is.null(wanted)){
    message("getting stored inverse")
    return(wanted)
  }
  
  ## otherwise calculate inverse, store it, return it
  
  data <- x$get()
  
  ## this is the resource hogging calculation
  
  wanted <- solve(data,...)
  
  ## this stores the inverse
  
  x$setsolve(wanted)
  
  ## return what we are after
  
  wanted
  
}
