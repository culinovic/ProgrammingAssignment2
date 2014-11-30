## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        imtx=NULL
        set=function(y){
                x<<-y
                imtx<<-NULL
        }
        get=function() x
        setimtx=function(inverse) imtx  <<- inverse 
        getimtx=function() imtx 
        list(set=set,get=get,setimtx=setimtx,getimtx=getimtx)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imtx=x$getimtx()
        if(!is.null(imtx))return(imtx)
        mat.data=x$get()
        imtx=solve(mat.data, ...)
        x$setimtx(imtx)
        return(imtx)
}
