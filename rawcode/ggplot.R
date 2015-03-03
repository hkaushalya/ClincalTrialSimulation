#apply test
#all elements must be of same type
rm(list=ls())
set.seed(12345)
m <- matrix(data=cbind(rnorm(30,0), rnorm(30,2), rnorm(30,5)), nrow=30, ncol=3)
m
apply(m,1,mean) # row means: 1 is for row in matrix
apply(m,2,mean) # col means: 2 is for columns in matrix
apply(m,c(1,2), mean) # this does not make sense for a 2D matrix as the mean of an element is itself.
apply(m,c(2,1), mean) # same as above, but will return a the transpose

#custom functions
apply(m,2, function(x) length(x[x<0]))
apply(m,2, function(x) is.matrix(x))
apply(m,2, function(x) is.vector(x))  #x is a vector. nor a column matrix

sapply(1:3, function(x) mean(m[,x]))  # not very readable as m is retrived by lexical scoping
sapply(1:3, function(x, y) mean(y[,x]), y=m) #using optional arugments is neater

colnames(m) <- c('method1', 'method2', 'method3')
head(m)

df <- as.data.frame(m)
df
#to convert all to value vs method (2 columns for plotting)
dfs <- stack(df)
dfs
is.factor(dfs[,2])
unstack(dfs)

#ggplot
require(ggplot2)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3)
ggplot(dfs[dfs$ind=="method1",], aes(x=values)) + geom_density()
ggplot(dfs[dfs$ind=="method2",], aes(x=values)) + geom_density()
ggplot(dfs[dfs$ind=="method3",], aes(x=values)) + geom_density()
