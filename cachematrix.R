## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## m is used as a flag to check matrix status.
    ## m is NULL if the matrix has not been calculated (no cache)
    ## m will contain inverse matrix value if the matrix has been calculated (using solve function)
    m<-NULL
    ## set matrix value
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    ## get matrix value
    get<-function() x
    ## set inverse matrix  value
    setmatrix<-function(solve) m<<- solve
    ## get inverse matrix value
    getmatrix<-function() m
    ## function returns a list of sub functions
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get inverse matrix value and assign to m
    m<-x$getmatrix()
    ## if m is not NULL then use m and return value
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## if m is null then calculate the inverse matrix with solve function
    ## and assign to m and return m value
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}