df<- faithful
med<- median(df[,2])
print(med)
print(df$compared_to_median)

for(i in 1:nrow(df)){
  if(df$compared_to_median[i,2]== med){append(compared_to_median,"Median")}
    else if (df$compared_to_median[i,2]< med){append(compared_to_median,"Greater")}
    else{(df$compared_to_median[i,2]> med){append(compared_to_median,"Smaller")}}
    }
  
  