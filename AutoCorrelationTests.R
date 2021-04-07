"
Autocorrelation tests to evaluate if the answers are independent and identically distributed.
"

install.packages("tseries")
library(tseries)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

acf(df_E2$answer)


acf(df_E2[df_E2$microtask_id==10,"answer"])

