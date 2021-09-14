#Lab 2
name <- "Shahreen Saleem"
liuid <- "shasa455"
install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")
library(markmyassignment)


#TASK 1.1.1----------------------------------------------------------
sheldon_game <- function ( player1 , player2 ){
  if(player1 == "rock" || player1 == "paper" || player1 == "scissors" || player1 == "spock" || player1 == "lizard"){
    if(player2 == "rock" || player2 == "paper" || player2 == "scissors" || player2 == "spock" || player2 == "lizard"){
    alt <- c("rock","lizard","spock","scissors","paper")
    stopifnot ( player1 %in% alt , player2 %in% alt)
    alt1 <- which ( alt %in% player1 )
    alt2 <- which ( alt %in% player2 )
    if( any (( alt1 + c (1 ,3)) %% 5 == alt2 )) {
      return ( " Player 1 wins !" )
    } else {
      return (" Player 2 wins !" )
    }
    return (" Draw !")    
    }
    else{
      return("Terminating Function")
    }
  }
  else{
    return("Terminating Function")
  }

}
sheldon_game("lizard", "spock")

#TASK 1.2.1----------------------------------------------------------

library("zoo") 
my_moving_median <- function ( series , nn)
{
 return(rollmedian(series, k = nn))
}

x = 1:10
my_moving_median(x, n=2)

x = c(5,1,2,NA,2,5,6,8,9,9)
my_moving_median(x, n=2)

x = c(5,1,2,NA,2,5,6,8,9,9)
my_moving_median(x, n=2)  

#TASK 1.2.2----------------------------------------------------------
library(tidyverse)

for_mult_table <- function(i,j){
for(i in From:to){
  j = From
  for(j in From:to){
    cat(paste(i*j," "))
    j=j+1
  }
  cat("\n")
  i = i + 1
}
}

From = 1
to = 5
for_mult_table(From, to)

From = 10
to = 12
for_mult_table(From, to)

#TASK 1.2.3----------------------------------------------------------

cor_matrix <- function (ftns)
{
  return(cor(ftns))
}


data(iris)
cor_matrix(iris[,1:4])

data(faithful)
cor_matrix(faithful)

#TASK 1.3.1----------------------------------------------------------

find_cumsum <- function(x,N) {
  
  df<-cumsum(seq(x))
  idx<-which.max(df > N)
  while(df[idx] > N)
  {
    return(df[idx]) 
  } 
  return(sum(x))
}

x = 1:100
n=500
find_cumsum(x, n)

x = 1:10
n=1000
find_cumsum(x, n)

#TASK 1.3.2----------------------------------------------------------

From = 3
to = 5
i = From
j = to
while(i<=to){
  j = From
  while(j<=to){
    cat(paste(i*j," "))
    j=j+1
  }
  cat("\n")
  i = i + 1
}

#TASK 1.3.3----------------------------------------------------------

trial_division_factorization <- function(x, i=2, factors = NULL){
  if(x<i) factors
  else if(! x %% i) trial_division_factorization(x/i, i, c(factors, i))
  else  trial_division_factorization(x, i+1, factors)
}

x = 2^3 * 13 * 17 * 31
trial_division_factorization(x)

x = 47 * 91 * 97
trial_division_factorization(x)

#TASK 1.4.1----------------------------------------------------------

repeat_find_cumsum <- function(x,N) {
  repeat{
  df<-cumsum(seq(x))
  idx<-which.max(df > N)
  if(df[idx] > N)
  {  return(df[idx])}
  else{
    return(sum(x))
  }
  }
}
x = 1:100
n=500
repeat_find_cumsum(x, n)

x = 1:10
n=1000
repeat_find_cumsum(x, n)

#TASK 1.4.2----------------------------------------------------------

library("zoo") 
repeat_my_moving_median <- function ( series , nn)
{
  repeat{  
    return(rollmedian(series, k = nn))
    }
}

x = 1:10
repeat_my_moving_median(x, n=2)

x = c(5,1,2,NA,2,5,6,8,9,9)
repeat_my_moving_median(x, n=2)

x = c(5,1,2,NA,2,5,6,8,9,9)
repeat_my_moving_median(x, n=2)  

#TASK 1.5.1----------------------------------------------------------

env <- search()[length(search())]
env

in_environment <- function(eenvv)
{
  return(ls(eenvv))
}

funs <- in_environment(env)
funs[1:5]

#TASK 1.5.2----------------------------------------------------------

library(pryr)
where("sd")
where("read.table")
where("non_existant_function")

#TASK 1.6.1----------------------------------------------------------

cov <- function(dataa)
{
  return(sapply(dataa, function(x) sd(x) / mean(x)))
}

data(iris)
X <- iris[1:4]
cov(X)

X <- iris[3:4]
cov(X)

#TASK 1.7.1----------------------------------------------------------

moment <- function(i){
  function(x) sum((x - mean(x)) ^ i) / length(x)
}

m1 <- moment(1)
m2 <- moment(2)
m1(1:100)

m2(1:100)

#TASK 1.7.2----------------------------------------------------------



