## Allows matrix inverse to be stored in parent
## environment to save time recalculating.
## First: assign makematrix to a variable
## e.g. > a <- makematrix(YOURMATRIXNAME).
## Then either: Return the inverse with cachesolve
## e.g. > cachesolve(a) # returns inverse based on
## stored inverse (if not stored it calculates and stores) 
## Or: Provide a prior known inverse solution
## e.g. > a$setinverse(KNOWNINVERSE) # Caution this value is not checked

## A stored inverse can also be obtained with
## e.g. > a$getinverse()

makematrix <- function(xm = matrix()) { # accepts a matrix
        invs <- NULL # calling makematrix clears previous inverse
        set <- function(ym) { # lexical scoping... xm as argument
                xm <<- ym # assign matrix to xm in parent environment   
                invs <<- NULL # clear previous inverse in parent env
        }
        get <- function() xm # returns the matrix, cachesolve has access
        setinverse <- function(solve) invs <<- solve # stores solution
                # utilising solution from cachesolve or manual input
        getinverse <- function() invs # returns stored inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # list makes function accessible
}

cachesolve <- function(xm, ...) { # function to return matrix inverse
                        # the argument needs to be the variable that
                        # makematrix is assigned to
        invs <- xm$getinverse() # access stored inverse
        if(!is.null(invs)) { # If inverse is stored...
                message("getting cached data") # output message
                return(invs) # Return the inverse and exit function
        }
        data <- xm$get() # Obtains the matrix from makematrix
        invs <- solve(data, ...) # calculate inverse of the matrix
        xm$setinverse(invs) # uses setinverse to store solution
        invs # returns the solution from calculation
}
