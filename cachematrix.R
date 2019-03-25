###############################################################################################
##
## The two functions defined below in this file support computing and caching a inverse of 
## a matrix. These functions make sure the inverse is computed only once when the matrix is
## changed. If teh matrix is not changed it returns the pre-computed inverse of the matrix.
##
############################################################################################### 

############################################################################################### 
##  Function: makeCacheMatrix()
## 
##  Input:
##       matrix object (optional)
##
##  Description:
##       Defines get, set matrix functions, and getInv and setInv inverse matrix
##  functions. It creates cacheMatrix object as a list and returns it to teh caller.
##
##  Returns:
##          Returns a cachedMatrix object as a list
##
############################################################################################### 

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    
    set <- function(m) {
        x <<- m
        invMat <<- NULL
    }
    
    get <- function() {
        x  
    }
    
    getInv <- function() {
        invMat
    }
    
    setInv <- function () {
        if(is.null(invMat)){
            print("Computing a inverse Matrix")
            mat <- get()
            invMat <<- solve(mat)
        }
        
        return (invMat)
    }
    
    list(set = set, get = get, getInv = getInv, setInv = setInv)
}


############################################################################################### 
##  Function: cacheSolve()
## 
##  Input:
##       cacheMatrix object
##
##  Description:
##       This function computes the inverse matrix if it is not already computed. 
##  If it is already computed it returns the pre-computed inverse matrix.
##
##  Returns:
##          Inverse of the matrix in the cacheMatrix object
##
############################################################################################### 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    im <- x$getInv()
    if(!is.null(im)) {
        print("returning cached invMat")
        return (im)
    }
    
    im <- x$setInv()
}

############################### TESTING CODE ################################################

##Test 1
## Create an caheMatrix object, m1
m1 <- makeCacheMatrix()

##TEST 2
## Calling the cacheSolve on empty matrix throws an error
m1inv <- cacheSolve(m1) ## fails
m1inv <- m1$getInv()  ## returns NULL (it does not fail)

##TEST 3
## Calling the cacheSolve after setting matrrix in cahcheMatrix object to a square matrix 
## Computes the inverse matrix
mat1 <- matrix(1:4, 2, 2)
m1$set(mat1)
m1inv <- cacheSolve(m1)

##TEST 4
## Calling the cacheSolve after setting inverse matrrix in cahcheMatrix object
## Returns pre-computed  inverse matrix
m2inv <- cacheSolve(m1)
identical(m1inv, m2inv)

##TEST 5
## Calling setInv() externally also returns the pre-computed matrix
##
m1$setInv()
m1inv <- cacheSolve(m1)


##TEST 6
## Calling setInv() externally also returns the pre-computed matrix
##
m1$set(mat1)
m1inv <- cacheSolve(m1)
m1$setInv()


##TEST 7
## Calling getInv() returns the pre-computed matrix
##
m1$getInv()
