## These functions will inverse a matrix. 
#They will take a simple (for example) 2x2 matrix and convert it to 2x2^-1

#|a b| ^-1 = (ad-bc)^-1 *|d -b|
#|c d|                   |-c a|

#The first function, makeVector creates a special "vector", which is really a 
#list containing a function to

#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

#The following function calculates the mean of the special "vector" created with 
#the above function. However, it first checks to see if the mean has already 
#been calculated. If so, it gets the mean from the cache and skips the 
#computation. Otherwise, it calculates the mean of the data and sets the value 
#of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

##This first function is designed to get/set the matrix and to 
#get/set the inverse matrix
#Using the MakeVector function as the backbone with minor adjustments

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculates the inverse for each position of the matrix
#using the previously mentioned mathematical method
#Using the cachemean function as the backbone with minor adjustments

cacheSolve<- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #using solve to find inverse from suggestion below
        x$setinverse(m)
        m
}
#SUGGESTION from coursera
#Computing the inverse of a square matrix can be done with the solve function 
#in R. For example, if X is a square invertible matrix, then solve(X) 
#returns its inverse.



#############SANITY CHECK#########################
tst <- matrix(c(1:4),2,2)
tst
tst2 <- makeCacheMatrix(tst)
cacheSolve(tst2)
inv <- (4-6)^-1 #4=1*4 and 6=3*2

1*inv
-2*inv
-3*inv
4*inv






