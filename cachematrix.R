## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#set-Assign the input argument to the x object in the parent environment, and
#Assign the value of NULL to the minverse object in the parent environment. 
#This line of code clears any value of minverse that had been cached by a prior execution of cachesolve().
#get-defines the getter for the matrix x
#setsolve-defines the setter for the solve of minverse
#getsolve-defines the getter for the solve of minverse

makeCacheMatrix <- function(x = matrix()) {
  minverse<-NULL
  set<-function(y){
    x<<-y
    minverse<<-NULL
  }
  get<-function()
    x
  setsolve<-function(solve)
    minverse<<-solve
  getsolve<-function()
    minverse
  list(set=set,get=get,
       setsolve=setsolve,getsolve=getsolve)
}

# used to get the cache of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse<-x$getsolve()
  #if the inverse exists, it gets it.
  if(!is.null(minverse)){
    message("getting cached data")
    return(minverse)
  }
  #if the inverse if not there, first it is calculated and then retrieved.
  data <- x$get()
  minverse <- solve(data, ...)
  x$setsolve(minverse)
  minverse
}
