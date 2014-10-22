## In this file, there are two functions demonstrating/implementing generating an inversed matrix via functions about creating/caching/getting a cached value of inversed matrix that was calculated before while doesn't recalculate it again, it's helpful sepecially when the calculation is time consuming.
# And it's also an example not only for matrix but also shows the idea about caching values for any kind of objects.
# With line of comments, the file readers could understand more about the details and there are steps' description at the final part of this file about how to test the two functions with a real invertible matrix object case, which also helps.
# Any feedback is welcome and thanks reading it. Its copyright is not reserved, you can modify/spread it freely:)

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# In fact, makeCacheMatrix returns a list which contains 4 functions: set, get, setIvdMatrix, getIvdMatrix. see descriptions below.
makeCacheMatrix <- function(x = matrix()) {
        ivdMatrix <- NULL
        # Function "set" sets a new matrix object with the list object.
        set <- function(y) {   
                x <<- y
                ivdMatrix <<- NULL
        }
        
        # Function "get" gets the current matrix object in the list object.
        get <- function() x
        
        # Function "setIvdMatrix" sets directly, a new inversed matrix object to be as cached matrix. 
        # Notice: The value is not caculated. Refer to function "cacheSolve" to caculate and get inversed matrix.
        setIvdMatrix <- function(IvdMatrixObj) ivdMatrix <<- IvdMatrixObj
        
        # Function "getIvdMatrix" gets the current inversed matrix object cached in the list.
        getIvdMatrix <- function() ivdMatrix
        
        # Return the list
        list(set = set, get = get, setIvdMatrix = setIvdMatrix, getIvdMatrix = getIvdMatrix)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by function makeCacheMatrix defined above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ivdMatrix <- x$getIvdMatrix() # Gets the current inversed matrix object in the list. 
        
        # If there IS such cached inversed matrix object, then return it directly and outputing one line of message.
        if(!is.null(ivdMatrix)) {  
                message("Getting cached Inversed Matrix")
                return(ivdMatrix)
        }
        
        # Otherwise, If there is NOT such cached inversed matrix object, then caculate it based on the given matrix object,
        # And sets the new caculated inversed matrix object as cached one with the given matrix object in the list.
        data <- x$get()
        ivdMatrix <- solve(data, ...) #Calculate 
        x$setIvdMatrix(ivdMatrix)   # Set it as the cached one
        
        # Returning the new caculated inversed matrix object.
        ivdMatrix                               
}

#       # How to run Test Case against the two functions:
#       1. Source the code file by running:  
#          > source("cachematrix.R")
#       2.  Create a matrix manualy, notice it must be invertible.
#          mTest <- matrix(data=c(4,3,3,2),nrow=2,ncol=2)
#       3. Check it's value is correct
#       > mTest
#            [,1] [,2]
#       [1,]    4    3
#       [2,]    3    2
#           
#       4. Get the inversed matrix by running solve function provided by R, and remember the output.
#          > solve(mTest)
#            [,1] [,2]
#       [1,]   -2    3
#       [2,]    3   -4
#       
#       5. use Function makeCacheMatrix to get the list with structure of internal functions and the cached inversed matrix.
#       #Notice: there's not a real inversed matrix ba calculated and cached, but only get a list returned as the output object, which then will be the argument for function cacheSolve to calculate.

#       mList<-makeCacheMatrix(mTest)
#       
#        6.  use Function cacheSolve to get the inversed matrix
#       > cacheSolve(mList)
#            [,1] [,2]
#       [1,]   -2    3
#       [2,]    3   -4
#       
#       for now, we know the function cacheSolve could generate a correct inversed matrix as solve function does.
#       
#       7. running cacheSolve again, check whether it can get the cached inversed matrix directly rather than re-calculate it again.
#       >  cacheSolve(mList)
#       Getting cached Inversed Matrix  
#            [,1] [,2]
#       [1,]   -2    3
#       [2,]    3   -4
#       
#       Notice, there's one more message line output "Getting cached Inversed Matrix " which means it evaluate the logic block of getting the cached inversed matrix while not re-calculate it.
#       
#       8. set a pseudo matrix as an cached inversed matrix for the original list
#       mFakeInvMatrix <- matrix(data=c(1,2,3,4),nrow=2,ncol=2)
#       > mFakeInvMatrix
#            [,1] [,2]
#       [1,]    1    3
#       [2,]    2    4
#       
#       mList$setIvdMatrix(mFakeInvMatrix)
#       
#       9. running cacheSolve again, check whether it go through logic block of getting the inversed matrix, since we gave it an inversed matrix value, though it's fake, but the function should output "Getting cached Inversed Matrix" and output the false inversed matrix.
#       > cacheSolve(mList)
#       Getting cached Inversed Matrix
#            [,1] [,2]
#       [1,]    1    3
#       [2,]    2    4
#       
#       Conclusion: two functions work well.

