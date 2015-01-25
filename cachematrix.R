## These two functions together will compute the inverse of matrix 'x' and cache it 
##for reference at a later time/inside other functions. 

## makeCacheMatrix is a list object that can cache the inverse of 'x' - the original
##  input matrix 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x<<-y
                i<<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" 
##  created with the function above. 


cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                data<-x$get()
                i<-solve(data, ...)
                x$setinverse(i)
                i
        }
}
