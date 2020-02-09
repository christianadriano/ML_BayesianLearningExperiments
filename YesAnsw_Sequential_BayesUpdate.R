" 
Bayes update assuming each question has 21 possible hypotheses.

H0 - zero number of YES's out of 20
H1 - One Ye's out of 20
H2 - two Yes' out of 20
...
H20 - all Ye's

Prior for each hypothesis= 1/21

"

library(stringr)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

task_id = 1
df <- df_E2[df_E2$microtask_id==task_id,]

#Since it is sampling without replacement, 
#I need to remove the answer from the list
sample_without_replacement <- function(){
  index = trunc(runif(1,1,20))
  
  sample <- df$answer[index]
  df <- df[-c(index),]
  if(sample=="YES_THERE_IS_AN_ISSUE")
    return(1)
  else
    return(0)
}


j=1

for(j in 1:6){
  n <- n+1
  h <- h + sample_without_replacement()
  print(h)
  
  
  
}