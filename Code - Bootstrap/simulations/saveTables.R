# Process mses
library(xtable)
df1 <- read.csv("Output/mse_simulation1.csv", colClasses = c("X" = "NULL"))
df2 <- read.csv("Output/mse_simulation2.csv", colClasses = c("X" = "NULL"))
df3 <- read.csv("Output/mse_simulation3.csv", colClasses = c("X" = "NULL"))
colnames(df1) <- c("variable", "sim1")
df1$sim2 <- df2$value
df1$sim3 <- df3$value
df1 <- df1[df1$variable != "mse.cate.oracle", ]

# Convert the dataframe to a LaTeX table
my_latex_table <- xtable(df1, caption = "MSE", label = "tab:simulatio_result")

# Print the LaTeX table
print(my_latex_table, include.rownames = FALSE, booktabs = TRUE)
write.csv(df1, "SimMSEResult.csv", row.names = FALSE)