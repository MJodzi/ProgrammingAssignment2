## This assignment will cache the inverse of a matrix for the R Programming 
## class on Coursera.

## This first function is meant to create a particular matrix that will record 
## its inverse. It incoporates the <<- operator to assign a "universal" object that 
## can then be used no matter what environment you are currently in.

makeCacheMatrix <- function(x = matrix()) {
        My_inv = NULL
        set = function(y) {
                x <<- y
                My_inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) My_inv <<- inverse
        getinv = function() My_inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
        ## In this first function we set/ get the matrix, and then set/get the  
        ## inverse. Finally the "list" at the end is to prepare it for the second step 
        ## "cacheSolve."

}


## The next function is the complimentary step of the first process, and  
## computes the inverse or simply returns what was computed by the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.null(My_inv)){
                message("Fetching Data For You")
                return(My_inv)
        }
        
        mat.data = x$get()
        My_inv = solve(mat.data, ...)
        x$setinv(My_inv)
        
        return(My_inv)
        
        ## The if statement can be read as: if the inverse was already calulated, skip
        ## the step that calculates it again, and instead simply post it by 
        ## returning My_inv. It will also give you the message "Fetching Data For You"
        ## so the user will know what is happening.
        
}
