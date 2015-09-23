#######################################################
##Functions created by Orlin Ariel Corea G. 23/09/2015
######################################################

##############################################################
##This function create a list of functions to process a matrix
##############################################################

makeCacheMatrix <- function(x = matrix()) {
 ##Variable that will contain the inverse matrix
    im<-NULL  
   #set the internal variable to given matrix
    set<-function(y)
    {
      x<<-y
      im<<-NULL
    }
    #get/return the matrix given
     get<-function()x
     #get the stored result of the inverse matrix
     getinverse<-function() im
     #set a value for the inverse matrix
     setinverse<-function(inverse)
     {
       im<<-inverse 
     }
     #define the list that contains all the functions defined  
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


################################################################
##This function is use to calculate the inverse of a Matrix
###############################################################
cacheSolve <- function(x, ...) {
   ##evaluate if the inverse has been already calculated
   inv<-x$getinverse()
   if(!is.null(inv))
   {
      message("getting cache data")
      return(inv)
   }
   ##if the inverse has not been calculated proceed to do it
   data<-x$get()
   inv<-solve(data,...)
    ##set the value of the inverse matrix for the above function
    x$setinverse(inv)
    ##return the result of the inverse matrix
    return(inv)
}

###########################################################################
## EXAMPLE OF USE (Just type the next command in the console in that order)
###########################################################################
## d<-makeCacheMatrix()
## d$set(matrix(1:4,nrow=2,byrow=TRUE))
## d$get()
## d$getinverse()
## cacheSolve(d)
## cacheSolve(d)
## d$getinverse()
