
## Put comments here that give an overall description of what your
## functions do

## Create and save the initial matrix

makeCacheMatrix <- function(x = matrix()){
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    get <- function() x 
    setInv <- function(Inverse)  Inv<<- Inverse
    getInv <- function() Inv
    list(set=set, get = get, setInv = setInv, getInv = getInv)
}


# If it is the first time the cacheSolve is called for a certain matrix
#The inverse is calculated and saved
#otherwise, return the Inverse of that matrix

cacheSolve <- function(x = matrix()){
    Inv <- x$getInv()
    if(!is.null(Inv)){
        message("Getting Cahced Data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data)
    x$setInv(Inv)
    Inv    
    ## Return a matrix that is the inverse of 'x'
}

#Testing below
# testMatrix <- matrix(c(5,36,21,235,6,7,99,123,23), nrow = 3, ncol = 3)
# testMatrix
# solve(testMatrix)
# 
# m <- makeCacheMatrix(testMatrix)
# cacheSolve(m)
