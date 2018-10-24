## Put comments here that give an overall description of what your
## functions do

        ## This set of functions takes as argument a given matrix, looks for the inverse
        ## in the cache, if it is not in cache calculates the inverse and, finally, 
        ## shows the result of the inverse on console. Also they set the usual getters 
        ## setters for the matrix and also its inverse.


## Write a short comment describing this function
        
        ## This function creates the "special" matrix. It contains the common group of
        ## getters and setters for a given matrix and also for its inverse.

makeCacheMatrix<-function(x=matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        
}


## Write a short comment describing this function

        ## This function searchs for the inverse of the given matrix in the cache. If it
        ## is stored, this function just recover the value. If it is not stored, then
        ## the inverse is calculated. At the end, value appears in console.


cacheSolve <- function(x,...){

        ## Return a matrix that is the inverse of 'x'        
        inv<- x$getInverse()
        if(!is.null(inv)){
                message("Getting Inverse from Cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
}


