## This is my third week assignment and the functions below (a) creates a caching matrix
## and (b) computes the inverse of output matrix as given by previous function in a)

## This function creates an object of the <<- type, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        ##  assumption is that 'x' is a square invertible matrix
        
        inv=NULL

        set=function(p){ ## 1. Set the matrix
        x<<-p
        ## I'm using the <<- operator to assign value to an object that's in a different
                ## environment from the current one.
        inv<<-NULL
        
        }
        get=function() x ## 2. Get the matrix
        setinv=function(inverse) ## 3. Set the inverse
        inv<<-inverse
        getinv=function() inv  ## 4. Get the inverse
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function returns the inverse of the matrix supplied to the function above (i.e. supplied to makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv=x$getinv()
        
        ## Case where inverse is calculated
        if(!is.null(inv)) {
        message("Fetching the cached data...please wait")
        return(inv) ## Skipping the computation and bringing the cached inverse
        }
        
        ## Case where inverse is not calculated
        else
                mat.data=x$get()
                inv=solve(mat.data,...)
        x$setinv(inv) ## Using the setinv function to assign the value of the inverse in the cache
        return(inv)
}
