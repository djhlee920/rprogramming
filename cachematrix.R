
## makeCacheMatrix function is for creating a special matrix object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        iv<-NULL
        set<-function(y){
                x<<-y
                iv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) iv <<-inverse
        getinverse<-function() iv
        list(set=set, get=get,
             setinvrese=setinverse,
             getinverse=getinverse)
}


## This function calculates the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv<-x$getinverse()
        if(!is.null(iv)){
                message("getting cached data"
                return(iv)
        }
        mydata<-x$get()
        iv<-solve(mydata,...)
        x$setinverse(iv)
        iv
}
}
