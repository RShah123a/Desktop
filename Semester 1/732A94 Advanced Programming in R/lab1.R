# Lab 1
name <- "Shahreen Saleem"
liuid <- "shasa455"

#------------------------------------
install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")

library(markmyassignment)
lab_path <-  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
mark_my_assignment()


#----------------------------------------------------
# my_num_vector()
my_num_vector <- function(){
  var1 <- c(log(11,10), cos(pi/5),exp(pi/3),((1173 %% 7)/19))
  return(var1)
  }
my_num_vector()

#---------------------------------------------------

#filter_my_vector(x, leq)

filter_my_vector <- function(x,leq){
  var2<-x>=leq
    return(var2)}
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)
  

#----------------------------------------------------

# dot_prod(a,b)

dot_prod <- function(a,b){
  var3<-sum(a*b) 
  return(var3)}
dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
dot_prod(a = c(-1,3), b = c(-3,-1))

#---------------------------------------------------

# approx_e()

approx_e<- function(N){
  var4<-sum(1/factorial(0:N))
  return(var4)}
approx_e(N=2)
approx_e(N=4)

#-----------------------------------------------------

# my_magic_matrix()

my_magic_matrix<- function(){
  var5<-matrix(c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3)
  return(var5)}
my_magic_matrix()

#-------------------------------------------------------

#calculate_elements(A)

calculate_elements<- function(A){
  return(length(A))}

mat <- my_magic_matrix()
calculate_elements(A = mat)

new_mat <- cbind(mat, mat)
calculate_elements(A = new_mat)

#-----------------------------------------------------

#row_to_zero(A, i)

row_to_zero <-function (A,i){
  A [i,] <- 0
  return(A)
}
mat <- my_magic_matrix()
row_to_zero(A = mat, i = 3)

row_to_zero(A = mat, i = 1)

#-------------------------------------------------

# add_elements_to_matrix(A, x, i, j)

add_elements_to_matrix <-function(A, x, i, j){
  A[i,j] = A[i,j] + x
  return(A)
}
mat <- my_magic_matrix()
print (mat)
add_elements_to_matrix(A = mat, x = 10, i = 2, j = 3)

add_elements_to_matrix(A = mat, x = -2, i = 1:3, j = 2:3)

#-----------------------------------------------------
# my_magic_list()

my_magic_list <-function(){
  var6 <- list("my own list", my_num_vector(),  my_magic_matrix())
  names(var6) <- c("info", "", "")
  return(var6)}
my_magic_list()
print(var6)

#-----------------------------------------------------
#change_info(x, text)

change_info <-function(x,text){
  a_list <- my_magic_list()
  a_list[1] <- "some new info"
  print(a_list)
  return (a_list)}
change_info(x = a_list, text = "Some new info")
print(a_list)

#----------------------------------------------------
# add_note(x, note)

add_note <-funtion(x, note){
  a_list <- my_magic_list()
  a_list[4] <- "This is a magic list!"
  name(a_list) <-c("info", "", "", "$note")
  print (a_list)
  return (a_list)}
add_note(x, note)
print(a_list)

#----------------------------------------------------------
# sum_numeric_parts(x)






#-------------------------------------------------------------
# my_data.frame()

my_data.frame <-function(){
var8 <- id <- c (1:3) 
name <- c("John", "Lisa", "Azra")
income <- c(7.30,0.00,15.21)
rich <- c("False", "False", "True")
return(var8)}
print(df)

#---------------------------------------------------------
# sort_head(df, var.name, n)

sort_head<- function(df, var.name, n) {
new_data <- df[order(df[,var.name], decreasing = TRUE),]
return(new_data[1:n,])}
#sorted_df<- sort_head(df, "Petal.Length", n)

#----------------------------------------------------------
# add_median_variable(df, j)

add_median_variable<- function(df, j){
  med_1<- median(df[ ,j])
  compared_to_median<-c() 
  for(i in 1:1nrow(df)){
    if(df[i, j]> med_1){
      compared_to_median<- append(compared_to_median,"Greater")
    }
    else if (df[i, j]< med_1){
      compared_to_median<- append(compared_to_median, "Smaller")
    }
    else {
      compared_to_median<- append(compared_to_median, "Median")
    }
  }
  return(cbind(df, compared_to_median))
}

data(faithful)
head(add_median_variable(df = faithful, 1))

#---------------------------------------------------------------------
# analyze_columns(df, j)

analyze_columns <- function(df, j){
  mean_1 <- mean(df[,j[1]])
  mean_2 <- mean(df[ ,j[2]])
  median_1 <- median(df[ ,j[1]])  
  median_2 <- median(df[ ,j[2]])
  std_1 <- sd(df[ ,j[1]]) * sqrt((length(df[ ,j[1]])-1)/length(df[ ,j[1]]))
  std_2 <- sd(df[ ,j[2]]) * sqrt((length(df[ ,j[2]])-1)/length(df[ ,j[2]]))
  correlation_matrix_1 <- mean_1/std_1
  correlation_matrix_2 <- mean_2/std_2
  b<- list(mean_1, median_1, std_1)
  c<- list(mean_2, median_2, std_2)
  names(b) <- c("mean", "median", "sd")
  names(c) <- c("mean", "median", "sd")
 a<- list(b,c,list(correlation_matrix_1, correlation_matrix_2))
 names(a) <- c(colnames(df)[j[1]], colnames(df)[j[2]], "correlation_matrix")
 return(a)}
data(faithful)
analyze_columns(df = faithful, 1:2)


