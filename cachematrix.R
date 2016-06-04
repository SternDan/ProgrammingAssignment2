## The functions makeCacheMatrix and cacheSolve work together to calculate
## the inverse of a matrix and return the value of the calculation. 
## To save computational time, the inverse is only calculated if the matrix
## has changed. Else, the results which have been cached upon the initial run
## are read from the cache.
## 
## Usage
## 1.   Source the functions into the working environment (make sure, that the
##      function is saved in the current working directory):
##      > source("cachematrix.R")
## 2.   Create a testmatrix. The matrix has to be invertible, that means that
##      the number of columns and rows have to be the same. However, a small
##      "if loop" has been integrated to catch this possible mistake.
##      > testmatrix <- matrix(runif(100, 1, 100), 10, 10)
##      This generates a 10 x 10 testmatrix filled with random numbers
## 3.   Now, initialize the first function "makeCacheMatrix" to be able to 
##      access the list of functions, that is returned.
##      > cachelist <- makeCacheMatrix()
## 4.   Now you are able to access the four functions, that are saved within
##      the list. As they are saved within a list with names, one can simply
##      use them by calling them with the "$" sign.
## 5.   Initialize the first function:
##      > cachelist$set(testmatrix)
##              ALTERNATIVE: you can also directly use the first function
##              on the testmatrix by calling:
##              > cachelist <- makeCachematrix(testmatrix)
## 6.   Now invert the testmatrix by using the second function:
##      > cacheSolve(cachelist)
## 7.   If the same command is rerun, the output "getting cached data" 
##      should appear. This means, that no calculation has been done but 
##      the data have been read from the cache.
## 8.   Now, to test if the results are correct, write the results in s matrix
##      invertedmatrix <- cacheSolve(cachelist)
## 9.   Calculate the cross product and round the output. You would expect a 
##      matrix of 0 with only the diagonal beeing 1
##      > round(invertedmatrix %*% testmatrix)
## 10.  Finally, generate a new testmatrix and rerun the following comands.
##      > testmatrix <- matrix(runif(100, 1, 100), 10, 10)
##      > cachelist$set(testmatrix)
##      > cacheSolve(cachelist)
##      The lack of the output "getting cached data" indicates, that a new
##      calculation was neccessary!


## makeCacheMatrix takes a matrix as input and initialize a set of
## function to calculate the inverse (solve) of a matrix and to write the
## results to the cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses the functions, that have been defined in the makeCacheMatrix
## to perform the calculations (if is hasn't been done before) or to read the
## results from the cache (if the calcuations have been done AND the matrix is
## unchanged)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if (nrow(data) == ncol(data)){
                i <- solve(data, ...)
                x$setinverse(i)
                i    
        }
        else {
                print("Matrix is not invertable")
        }
        
}
