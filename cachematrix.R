## Assignment: Caching the Inverse of a Matrix
## Author: Rikesh Thapa
## Date: 9/24/2016
## Description of assignment:
##      - This script is comprised of 2 functions:
##        1) makeCacheMatrix: Creates a special matrix object 
##        that can cache its inverse
##        2) cacheSolve: Calclates the inverse of the special 
##        matrix created in the makeCacheMatrix. In the case that
##        the inverse has already been calcualted for the given
##        matrix cacheSolve will just look in teh cache and return 
##        the previously calculate inverse
##


## As described above, makeCacheMatrix creates a special matrix 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setSolve <- function(solve) m<<- solve
  getSolve <- function() m
  list(set = set, get=get,
       setSolve= setSolve,
       getSolve = getSolve)
}

## As mentioned in the intro, cacheSolve computes the inverse of the
## special martrix and if the value has already been calculated,
## retrieves teh cache.


cacheSolve <- function(x, ...) {
  m<- x$getSolve()
  if(!is.null(m)){
    message("get the cached value")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setSolve(m)
  m   ##return m which is inverse of x
}
