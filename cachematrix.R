## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition. This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(X, ...) 
{
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is gonna be computed")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}

R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' 