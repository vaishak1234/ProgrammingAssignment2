## The two functions below help in saving processing power and improve efficiency
## by returning value of inverted matrices from cache memory if already present

## This function helps in setting the value of the matrix, getting the value of the matrix,
## setting the inverse of the matrix, and retrieving the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  Matrix_inv <- NULL
  
  set <- function(y)                        #Sets the value of the matrix
    {
      x <<- y
      Matrix_inv <<- NULL
    }
 
  get <- function() x                       #Gets the Value of the matrix                        
  setinverse <- function(z)                 #Set the inverse of the matrix
    { 
      Matrix_inv <<- z  
    }
    
  getinverse <- function() Matrix_inv       #Gets the inverse of the matrix                  
  list(set = set, get = get,
  setinverse = setinverse, getinverse = getinverse) #Creating a list with the four values

}


## This function helps in returning the inverse of the matrix returned by MakeCacheMatrix 
## function. If the inverse has been calculated earlier, the function would retrieve the 
## inversed matrix from Cache memory

cacheSolve <- function(x, ...) 
{
   Matrix_inv <- x$getinverse()             #Gets the inverse of the matrix
   if(!is.null(Matrix_inv)) 
     {                      
      message("Getting Cached Inverted Matrix")      #If the inverse present, display message and
      return(Matrix_inv)                             #return the inversed matrix from cache memory      
     }
  
   Matrix <- x$get()                                      
   Matrix_inv <- solve(Matrix,...)          #If inverse of matrix not present, input matrix is solved to compute inverse          
   x$setinverse(Matrix_inv)                 
   return(Matrix_inv)    
}

#Test Example

InputMatrix <- matrix(c(1,4,6,8,9,11,12,16,20),3,3)
InputMatrix

Cachematrix <- makeCacheMatrix(InputMatrix)
Cachematrix$get()
Cachematrix$getinverse()
cacheSolve(Cachematrix) # To call the function first time 
cacheSolve(Cachematrix) # Calling the function second time to invoke Cached inverted Matrix 
