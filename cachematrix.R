## The following two functions make use of the scoping rules of R to cache the
## inverse of a matrix, it is assumed that the matrix is square and invertible

## The first function creates a special "matrix" which is a list containing 
##functions that can be used to store and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
         inv<-NULL
         #the following function sets the value of the matrix, the operator <<-
         #allows the value of the matrix to be defined in an environment  
         #different to the current one
         set<-function(y){
            x<<-y
            inv<-NULL
         }
         #the following function is used to retrieve the value of the matrix 
         get<-function() x
         #the following function is used to set the value of the inverse and 
         #this value could be obtained from an environment different to this one
         setinverse<-function(invert) inv<<-invert
         #the following function allows the inverse to retrieved
         getinverse<-function() inv
         list(set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
}


## The following function computes the inverse of the "special" matrix created
##with the above function, but it first checks whether or not the inverse has 
##already been calculated. If it has been calculated, it calls the cached value of
##the inverse

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if (!is.null(inv)){
          #if the value of inverse obtained from getinverse is not null 
          message("getting cached inverse")
          ## Return a matrix that is the inverse of 'x
          return(inv)
        }
        #else calculate inverse and store it using setinverse
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}
