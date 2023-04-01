library ( rpart )
library ( rpart.plot )
set.seed (2001) # random seed
n <- 200
maxDigit <- 2
y <- rep (0: maxDigit , length = n) # labels , replicates the values 
# patterns o f the digits
pattern <- c (1 ,1 ,1 ,0 ,1 ,1 ,1 ,
               0 ,0 ,1 ,0 ,0 ,1 ,0 ,
               1 , 0 , 1 , 1 , 1 , 0 , 1 ,
               1 , 0 , 1 , 1 , 0 , 1 , 1 ,
               0 , 1 , 1 , 1 , 0 , 1 , 0 ,
               1 , 1 , 0 , 1 , 0 , 1 , 1 ,
               0 , 1 , 0 , 1 , 1 , 1 , 1 ,
               1 , 0 , 1 , 0 , 0 , 1 , 0 ,
               1 , 1 , 1 , 1 , 1 , 1 , 1 ,
               1 , 1 , 1 , 1 , 0 , 1 , 0 )
lights <- matrix ( pattern , 10 , 7 , byrow = TRUE ) # patterns a s a matrix
lights<- lights [1: ( maxDigit +1) ,] # select just some digits
noisy_lights <- matrix ( rbinom ( n*7, 1, 0.8) , n, 7) # Noisy lights
noisy_lights <- ifelse ( lights[ y +1 , ] == 1 , noisy_lights , 1 - noisy_lights )
random_lights <- matrix ( rbinom ( n*17, 1, 0.5) , n , 17) # Random lights
x <- cbind (noisy_lights , random_lights ) # inputs . Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively.
training_data <- as.data.frame(x) # training dataset : inputs
training_data$y <- y # training dataset : labels

# 1. Fit a decision tree with the "rpart" function using the Gini index. The tree wil be called
# "treeFitted".

treeFitted <-rpart(y~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24,data=training_data,method="class")
#the default parameter is gini

# 2. Plot the decision tree with "rpart.plot(treeFitted,type=3)". Propose a brief interpretation of
# the decision tree.

rpart.plot(treeFitted,type=3)
#The percentage shows the gini index, the color the final category, the branches the variable used to decide.
#the three decimal numbers how many samples of y=0,1,2 are finally classified into what category

# 3. Run the command "printcp(treeFitted)". What is the root node error ? What are CP, nsplit,
# rel error, xerror, and xstd in the last list printed by "printcp" ? How to interpret this list ? Is
# this list useful ? This list is also given by "treeFitted$cptable"

printcp(treeFitted)

# one row, one 
#cp = complexity parameter, decreases as you go down
# nsplit=number of splits in the level 
# rel_error= relative error, decreases as you go down
# xerror = cross-validation error, increases as you go down
# xstd = standard deviation

#Rule of thumb choose the lowest level where the rel_error + xstd < xerror.

#Root node error: 133/200 = 0.665
#   CP        nsplit rel_error  xerror     xstd
# 1 0.323308      0   1.00000 1.14286 0.045413
# 2 0.285714      1   0.67669 0.88722 0.052298
# 3 0.026316      2   0.39098 0.47368 0.049393   !!! we should stop here
# 4 0.010000      4   0.33835 0.45113 0.048727


plotcp(treeFitted)
#It shows the value of x-error (when to prune), size 3

# 5. What is a reasonable optimal complexity ? 
# Select with a R command this reasonable optimal complexity value.

#optimal_cp <-treeFitted[which.min(treeFitted$cptable[,4]),1]

which.min(treeFitted$cptable[,1])


# If the cost of adding another variable to the decision tree from the current node is above the value of cp, then tree building does not continue

# 6. Prune the decision tree "treeFitted" with the command "prune". You can use "prp" to visualize the pruned decision tree and compre it to "treeFitted".

treePruned <- prune(treeFitted, cp = optimal_cp )

rpart.plot(treeFitted,type=3)
rpart.plot(treePruned,type=3)

# 7. Code a R function to compute the accuracy of a decision tree. The R function has three
# arguments : the decision tree, the inputs and the labels. It returns the accuracy.

accuracy <- function(tree,x,y) {
  p <- predict(tree,x,type = 'class')
  table_mat <- table(y, p)
  acc <- sum(diag(table_mat)) / sum(table_mat)
  print(acc)
  return(acc)
}

# 8. Compute the accuracy of "treeFitted" and the pruned tree on the training set.

print(accuracy(treeFitted,as.data.frame(x),y))

print(accuracy(treePruned,as.data.frame(x),y))


# 9. Generate a test set composed of n0 = 200 test samples. Compute the accuracy of "treeFitted" and the pruned tree on the test set. Compare the performance of "treeFitted" and the
# pruned tree.




# 10. Repeat the previous question with n = 1000 and n0 = 400.


# 11. Conclude on the interest of the complexity value
