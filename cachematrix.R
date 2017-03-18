## Author: Vi Tran
## Date: March 18th, 2017
## Name of function: Caching the inverse of a Matrix

## makeCacheMatrix: creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setI <- function(y){
        x <<- y
        m <<- NULL
    }
    getI <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setI = setI,
         getI = getI,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the matrix. If inverse already exists,the inverse will be cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
   
    if (!is.null(m)){
        message("getting cached inverse matrix")
        return(m)
    }
    data<-x$getI()
    if(nrow(data) != ncol(data)){
        message("Matrix is not squared. Truncating matrix...")
        if(nrow(data) > ncol(data)){
            data <- data[1:ncol(data),]
        }else if (nrow(data) < ncol(data)){
            data <- data[,1:nrow]
        }
        m<-solve(data)
        x$setinverse(m)
        m
    } else {
        m<-solve(data)
        x$setinverse(m)
        m
    }
    
    
}

myMatrix <- makeCacheMatrix(matrix(sample(1:100),10,10))
cacheSolve(myMatrix)
myMatrix$setI(matrix(sample(1:50),5,5))
cacheSolve(myMatrix)
