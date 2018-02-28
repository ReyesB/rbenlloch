## Overall these two functions will calculate the inverse 
## of a matrix and cache the value in case it is needed again.
## The function will check if the inverse of the matrix is 
## calculated and stored snd if it that is the case, it will 
## recover the value instead of recalculating it again. 

## This fucntion creates a special matrix that contains a 
## function that calculates the inverse of teh matrix and
## stores it in cache for later use

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
get<-function()x
setInverse<-function(solveMatrix) inver<<-solveMatrix
getInverse<-function() inver
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function calcualtes the inverse of a matrix but 
## before doing so it checks whether this inverse matrix 
## has been calculated been and if that is the case it 
## recovers the value without the need to recalculate it again. 

cacheSolve <- function(x, ...) {
       inver<- x$getInverse()
       if (!is.null(inver)){
         message("getting cache data")
         return(inver)
       }
       data<-x$get()
       inver<-solve(data)
       x$setInverse(inver)
       inver
}
