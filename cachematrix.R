## makeCacheMatrix and cacheSolve are able to compute the inverse of a matrix. In case the inverse has already been calculated
## in this or another environment, they are able to chache the already calculated value.


## makeCacheMatrix takes a matrix and returns a list of functions used by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        ## Set the variable inv as NULL
        inv <- NULL
        
        ## The set function is used when you want to apply these functions a second time with another matrix.
        ## It sets as x the new matrix and gives to inv a value of NULL. The operator <<- is used in order to change the values in the environment where the functions where initially called
        ## In this way, an old saved value of inv is not chaced for the new matrix.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## The get function returns the matrix x
        get <- function() x
        
        ## The set inverse function stores the value of the argument used in the function (s) in the variable inv.
        ## It uses the <<- operator so that the inv value can be chanced in the future.
        setinverse <- function(s) inv <<- s
        
        ## The getinverse function returns the inv value. 
        getinverse <- function() inv
        
        ## The final result of the function is a list contaitning the functions: set, get, setinverse and getinverse. 
        ## They can be called as: x$set, x$get, x$setinverse, x$getinverse.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses the functions in the list of makeCacheMatrix to calculate the inverse of the matrix and, in case, chache its value.

cacheSolve <- function(x, ...) {
        ## It assigns the value stored in the getinverse function to the variable inv.
        inv <- x$getinverse()
        
        ## If the value inv is NOT null, it means that it was aleady calculated and set to be equal to inv.
        ## Therefore the message "getting chached data" appears and the inv value is returned.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the value inv IS null, it means that it was never calculated. Therefore the matrix is given to the variable "data",
        ## the inverse of the matrix "data" is calculated with the solve function and set to the variable inv.
        ## Then the function setinverse is called with the argument inv, so that it stores the argument inv of the function as the new value of inv that can be cached next time.
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        
        ## the inverse of the matrix is returned as result of the function.
        inv
}
