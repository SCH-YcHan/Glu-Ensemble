rm(list=ls())

library(ega)
library(dplyr)
library(tidyr)
library(gridExtra)

test_data <- data.frame()
for (i in 0:9) {
  t <- read.csv(paste0("../../data/BG LBStack/Test_Fold",i,"_lbstack.csv"))
  test_data <- rbind(test_data, t)
}

lr <- test_data$LR
lasso <- test_data$LASSO
y_true <- test_data$label

test_naive <- data.frame()
for (i in 0:9){
  t <- read.csv(paste0("../../data/BG Naive/Test_Fold",i,".csv"))
  test_naive <- rbind(test_naive, t)
}

test_naive <- test_naive %>% 
  group_by(file_name) %>% 
  mutate(y12_shifted = lag(y12, 6)) %>% 
  drop_na

avg_data <- data.frame()
for (i in 0:9) {
  avg_t <- read.csv(paste0("../../data/BG Avg/Test_Fold",i,"_avg.csv"))
  avg_data <- rbind(avg_data, avg_t)
}

avg <- avg_data$AVG

png("../EGA_LR.png", width=2200, height=2000, res=300)
lr_ega <- plotParkesGrid(
  referenceVals = y_true,
  testVals = lr,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [LR]",
  xlab = " "
)
lr_ega
dev.off()

png("../EGA_LASSO.png", width=2200, height=2000, res=300)
lasso_ega <- plotParkesGrid(
  referenceVals = y_true,
  testVals = lasso,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [LASSO]",
  xlab = " ",
  ylab = " "
)
lasso_ega
dev.off()

png("../EGA_NAIVE.png", width=2200, height=2000, res=300)
naive_ega <- plotParkesGrid(
  referenceVals = test_naive$y12,
  testVals = test_naive$y12_shifted,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [NAIVE]",
  ylab = " "
)
naive_ega
dev.off()

png("../EGA_AVG.png", width=2200, height=2000, res=300)
avg_ega <- plotParkesGrid(
  referenceVals = y_true,
  testVals = avg,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [AVG]",
)
avg_ega
dev.off()

png("../Fig7.png", width=4400, height=4000, res=300)
grid.arrange(lr_ega, lasso_ega, avg_ega, naive_ega, nrow=2, ncol=2)
dev.off()

zones_lr <- getParkesZones(
  referenceVals = y_true,
  testVals = lr,
  type=2
)
zones_lr <- factor(zones_lr, levels = c("A", "B", "C", "D", "E"))
EGA_table_lr <- data.frame(round(table(zones_lr)/length(zones_lr)*100, 3))
names(EGA_table_lr) <- c("Zone", "LR")
A_B <- data.frame(
  Zone = "A+B",
  LR = EGA_table_lr$LR[1] + EGA_table_lr$LR[2]
)
EGA_table_lr <- rbind(A_B, EGA_table_lr)


zones_lasso <- getParkesZones(
  referenceVals = y_true,
  testVals = lasso,
  type=2
)
zones_lasso <- factor(zones_lasso, levels = c("A", "B", "C", "D", "E"))
EGA_table_lasso <- data.frame(round(table(zones_lasso)/length(zones_lasso)*100, 3))
names(EGA_table_lasso) <- c("Zone", "LASSO")
A_B <- data.frame(
  Zone = "A+B",
  LASSO = EGA_table_lasso$LASSO[1] + EGA_table_lasso$LASSO[2]
)
EGA_table_lasso <- rbind(A_B, EGA_table_lasso)


zones_naive <- getParkesZones(
  referenceVals = test_naive$y12,
  testVals = test_naive$y12_shifted,
  type=2
)
zones_naive <- factor(zones_naive, levels = c("A", "B", "C", "D", "E"))
EGA_table_naive <- data.frame(round(table(zones_naive)/length(zones_naive)*100, 3))
names(EGA_table_naive) <- c("Zone", "NAIVE")
A_B <- data.frame(
  Zone = "A+B",
  NAIVE = EGA_table_naive$NAIVE[1] + EGA_table_naive$NAIVE[2]
)
EGA_table_naive <- rbind(A_B, EGA_table_naive)


zones_avg <- getParkesZones(
  referenceVals = y_true,
  testVals = avg,
  type=2
)
zones_avg <- factor(zones_avg, levels = c("A", "B", "C", "D", "E"))
EGA_table_avg <- data.frame(round(table(zones_avg)/length(zones_avg)*100, 3))
names(EGA_table_avg) <- c("Zone", "AVG")
A_B <- data.frame(
  Zone = "A+B",
  AVG = EGA_table_avg$AVG[1] + EGA_table_avg$AVG[2]
)
EGA_table_avg <- rbind(A_B, EGA_table_avg)

EGA_table_lr
EGA_table_lasso
EGA_table_avg
EGA_table_naive

EGA_table = cbind(
  EGA_table_lr,
  EGA_table_lasso %>% select(LASSO),
  EGA_table_avg %>% select(AVG),
  EGA_table_naive %>% select(NAIVE)
) %>% t %>% data.frame
names(EGA_table) <- NULL

LLN <- read.csv("../../data/BG LBStack Pred/Test_RMSE_lbstack.csv")
AVG <- read.csv("../../data/BG Avg Pred/Test_RMSE_avg.csv")

RMSE_table = rbind(LLN, AVG)
names(RMSE_table) <- c("Method", names(RMSE_table)[-1])

MEAN_SD <- function(x){
  mean_val = round(mean(x), 3)
  sd_val = round(sd(x), 3)
  mean_sd <- paste0(mean_val,"(", sd_val, ")")
  return(mean_sd)
}

RMSE_MEAN_SD <- RMSE_table %>% 
  select(-Method) %>% 
  apply(1, MEAN_SD)

RMSE_table["MEAN_SD"] = RMSE_MEAN_SD
RMSE_table <- RMSE_table[c(1,2,4,3),]
RMSE_table[,2:11] <- data.frame(lapply(RMSE_table[,2:11], function(x) round(x,3)))

print(RMSE_table)
print(EGA_table)

RMSE_table_t <- RMSE_table %>% 
  select(-Method, -MEAN_SD) %>% 
  t %>% 
  data.frame
names(RMSE_table_t) <- RMSE_table$Method

png("../Fig6.png", width=2000, height=2000, res=300)
boxplot(RMSE_table_t, ylab="RMSE", lwd=2, ylim=c(17, 30))
dev.off()
