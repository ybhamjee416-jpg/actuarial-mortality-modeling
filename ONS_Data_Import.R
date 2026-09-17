install.packages("readxl")
setwd("C:/Users/student/Desktop/Summer_Actuarial_Project_2026")
library(readxl)
life_table <- read_excel("nltuk198020223.xlsx", sheet = "2022-2024")
excel_sheets("nltuk198020223.xlsx")
View(life_table)
clean_table <- life_table[6:106, ]
colnames(clean_table)[1] <- "x"
colnames(clean_table)[3] <- "qx"
colnames(clean_table)[5] <- "lx"
colnames(clean_table)[6] <- "dx"
colnames(clean_table)[1:4] <- c("x", "qx", "lx", "dx")
View(clean_table)
head(clean_table[, 1:4])
clean_table <- life_table[6:106, ]
colnames(clean_table)[1] <- "x"
colnames(clean_table)[3] <- "qx"
colnames(clean_table)[5] <- "lx"
colnames(clean_table)[6] <- "dx"
View(clean_table)
clean_table <- life_table[6:106, ]
colnames(clean_table)[1] <- "x"   # Age
colnames(clean_table)[3] <- "qx"  # Mortality rate
colnames(clean_table)[5] <- "lx"  # Survivors (starts at 100,000)
colnames(clean_table)[6] <- "dx"  # Deaths
View(clean_table)
print(as.character(life_table[5, ]))
clean_table <- life_table[6:106, ]
colnames(clean_table)[1] <- "x"
colnames(clean_table)[3] <- "qx"
colnames(clean_table)[4] <- "lx"
colnames(clean_table)[5] <- "dx"
View(clean_table)
clean_table$lx <- as.numeric(clean_table$lx)
clean_table$qx <- as.numeric(clean_table$qx)
clean_table$dx <- as.numeric(clean_table$dx)
clean_table$lx[41]
t_p_40 <- clean_table$lx[41:101] / clean_table$lx[41]
plot(40:100, t_p_40, type = "l", col = "blue", 
     xlab = "Age", ylab = "Probability of Survival (t_p_40)",
     main = "Survival Curve for a 40-Year-Old Male")
e_40 <- sum(t_p_40[2:61])
print(e_40)
clean_table$ex <- 0
for (i in 1:101) 
  future_lx <- clean_table$lx[i:101]
  t_px <- future_lx / clean_table$lx[i]
  clean_table$ex[i] <- sum(t_px[-1])
head(clean_table[, c("x", "lx", "ex")])
clean_table$lx[1:10]
rev_lx <- rev(clean_table$lx)
rev_lx <- rev(clean_table$lx)
clean_table$ex <- (rev(cumsum(rev_lx)) / clean_table$lx) - 1

               