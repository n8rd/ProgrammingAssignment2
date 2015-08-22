##The functions makeCacheMatrix and cacheSolve take advantage of the scoping 
##rules of the R language to preserve state inside of an R object.
##These functions can be used to cache results of potentially time-consuming 
##computations such as calculating the inverse of a matrix

##Illustration:
##> mdat<-matrix(c(1,4,9,16),2,2)
##> mdat1<-makeCacheMatrix(mdat)
##> mdat2<-cacheSolve(mdat1)
##> mdat2$get()
##        [,1]  [,2]
##  [1,] -0.8  0.45
##  [2,]  0.2 -0.05
##> mdat2<-cacheSolve(mdat1)
##  getting cached data

## makeCacheMatrix - creates a special "matrix" object 
##                   that can cache its inverse

makeCacheMatrix <- function(mat = matirx()) {
  
  matinv <- NULL
  
  #set the value of matrix & set its inverse to NULLL
  set <- function(marg) {
    mat <<- marg
    matinv <<- NULL
  }
  #get the value of the matrix
  get <- function() mat
  
  #set the value of inverse of the matrix
  setmatinv <- function(minv) matinv <<- minv
  
  #get the value of inverse of the matrix
  getmatinv <- function() matinv
  
  
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)

}


## cacheSolve - computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. 
##              If the inverse has already been calculated 
##              (and the matrix has not changed), 
##              then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(mat, ...) {
  
  #check if the inverse of the matrix has been calculated already
  #if yes get the inverse from the cache and return
  m <- mat$getmatinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #value of m fetched above is NULL..
  #calculate the inverse of the matrix 
  #and set the value in the cache via the setmatinv function
  data <- mat$get()
  m <- solve(data, ...)
  mat$setmatinv(m)
  m
  
}