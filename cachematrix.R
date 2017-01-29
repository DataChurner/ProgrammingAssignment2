## The Funtions makeCacheMatrix and cachesolve 
## makeCacheMatrix - Makes a cacheable matrix out of the matrix supplied for which the inverse needs
## to be calculated
## 
## cachesolve - is a function that determines if the same matris is being passed, and returns a cached
## value if it is, or determines the inverse if it is not.
## 

makeCacheMatrix <- function(x = matrix()) {
        
        # Resetting the value of the matrix i with all NA
        i <- matrix(,nrow(x),ncol(x))
        
        # Set function with the argument being passed and the resetting of the value of i
        # from the previous envt.
        set <- function(y) {
                x <<- y
                i <<- matrix(,nrow(x),ncol(x))
        }
        
        # get function passing the argument from prior environment
        get <- function() x
        
        # setting the matrix information from the prior environment
        setinv <- function(solve) i <<- solve
        
        # getting the value of the matrix from the prior environment
        getinv <- function() i
        
        # tHIS is important step, which sets the output of the functions into the list output,
        # so that the values of the variables can be dteremined by invoking the $get,$set,
        # $setinv and $getinv of the variable the function output is assigned to
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # retreive the $getinv value to validate if its the first call or a repeated call
        # first call with an initialized matrix, repeated calls will have an inverse already.
        i <- x$getinv()
        
        # since i is a matrix, just the is.na does for get a logical output, it will be a matrix as well
        # all function is used to get a logical if all values are NA 
        # if its not a first time call
        if(all(!is.na(i))) {
                message("getting cached data")
                return(i)
        }
        
        #ELSE - if its a first time call
        
        # set the variable data to the Matrix
        data <- x$get()
        
        # pass the matrix to solve function, and get the inverse 
        i <- solve(data, ...)
        
        # Set the value of i so that its availavle to the makeCacheMatrix variable
        x$setinv(i)
        
        # return the inverse as a matrix
        i
}
