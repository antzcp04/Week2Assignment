#Suppose that you have five customers - James, Mary, Steve, Alex, and Patricia - in line at a store. Use R 
#operations to perform the following tasks in sequence.


#a. Assign the five individuals to a vector called queue
name <- c("James","Mary","Steve","Alex","Patricia")

#b. Update the queue for the arrival of a new patron named Harold.
name <- append(name,"Harold")

#c. Update the queue to reflect the fact that James has finished checking out.
name <- name[-1]

#d. Update the queue to reflect the fact that Pam has talked her way in front of Steve with just one 
#item.

nameOrig <- c("Mary","Steve","Alex","Patricia","Harold")

name <- c("Mary","Patricia""Steve","Alex","Harold")


nameNew <-""

nameNew[1] <- nameOrig[2]
nameNew[2] <- nameOrig[3]
nameNew[3] <- nameOrig[4]
nameNew[4] <- nameOrig[5]
nameNew[5] <- ""

nameNew[5] <- nameOrig[5]

getQueue <- function(4,2) {
  for (i in 1:5 ) {

    nameNew[i] <- nameOrig[i+1]

    
    if (  i > 5  )  {
      nameNew[5] <- ""      
    }
  }
}



#e. Update the queue to reflect the fact that Harold has grown impatient and left.
name <- c("Mary","Patricia","Steve","Alex","Harold")
name <- name[-5]

#f. Update the queue to reflect the fact that Alex has grown impatient and left. (Do this as if you do not 
                                                                                 know what slot Alex currently occupies by number.)

name <- c("Mary","Patricia","Steve","Alex","Harold")
 
getAlex <- function(name) {
  alex <- "Alex"
  for (i in 1:4 ) {
    if ( name[i] == "Alex" )     {
      name <- name[-i]
    }
  }
  print(name)
} 




#g. Identify the position of Patricia in the queue.

getPosition <- function(name) {
  patricia <- "patricia"
  for (i in 1:4 ) {
    if ( name[i] == "Patricia" )     {
      print(i)
    }
  }
  
} 
 

#h. Count the number of people in the queue
 
getNumber <- function(name) {
  c<- 0
  for (i in name ) {
    c <- c + 1   
    
  }
  print(c)
} 


#2) Modify your answer to quiz exercise 21 so that when you implement the quadratic equation, meaningful 
#output is given whether there are one, two, or no solutions. (Hint: Use the discriminant.)

getQuad <- function(a,b,c) {
  x <-    ( (-1*b)  -   sqrt((b^2)-(4*a*c)) )  / 2*a
  print (x)
}  

tr <- sample(1:50, 25)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
z <- qda(train, cl)
predict(z,test)$class
#3. Use R to determine how many numbers from 1 to 1000 are not divisible by any of 3,7, and 11.

getDivisible <- function() {
  d <- 0
  for (i in 1:1000 ) {
    q <- i %% 3  
     if (q != 0) {
       d <- d + 1     
       
     }
  }
  print(d)
} 

#4. Write R code that takes three input constants f, g, and h and determines whether they form a Pythagorean 
#Triple (such that the square of the largest input is equal to the sum of the squares of the other two 
#        constants).


getPythagoreanTriple <- function(a,b,c) {
  msg1 = "Valid Pythagorean Triple"
  msg2 = "InValid Pythagorean Triple"
  maxValue <- 0
  minVal1 <- 0
  minVal1 <-0
  if ( ( b < a ) &&  ( a > c ) ){
     maxValue <- a
     minVal1 <- b
     minVal2 <- c
     print("a")
  } else if ( ( a < b ) &&  ( b > c ) ) {
    maxValue <- b
    minVal1 <- a
    minVal2 <- c    
    print("b")
  } else if ( ( a < c ) &&  ( c > b ) ) {
    maxValue <- c
    minVal1 <- a
    minVal2 <- b    
    print("c")
  }
  
  if ( ( minVal1^2 + minVal2^2) == maxValue^2 ) {
    #print(minVal1^2)
    #print(minVal2^2)
    #print(maxValue^2)
    print(msg1)
  } else {
    print(msg2)
  }
  
} 


