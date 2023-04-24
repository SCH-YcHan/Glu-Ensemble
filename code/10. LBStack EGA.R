rm(list=ls())

library(ega)
library(dplyr)
library(tidyr)

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
plotParkesGrid(
  referenceVals = lr,
  testVals = y_true,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [LR]"
)
dev.off()

png("../EGA_LASSO.png", width=2200, height=2000, res=300)
plotParkesGrid(
  referenceVals = lasso,
  testVals = y_true,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [LASSO]"
)
dev.off()

png("../EGA_NAIVE.png", width=2200, height=2000, res=300)
plotParkesGrid(
  referenceVals = test_naive$y12_shifted,
  testVals = test_naive$y12,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [NAIVE]"
)
dev.off()

png("../EGA_AVG.png", width=2200, height=2000, res=300)
plotParkesGrid(
  referenceVals = avg,
  testVals = y_true,
  type = 2,
  title = "Parkes (Consensus) Error Grid for Type 2 Diabetes [AVG]"
)
dev.off()


zones_lr <- getParkesZones(
  referenceVals = lr,
  testVals = y_true,
  type=2
)
zones_lr <- factor(zones_lr)
table(zones_lr)
round(table(zones_lr)/length(zones_lr)*100, 3)


zones_lasso <- getParkesZones(
  referenceVals = lasso,
  testVals = y_true,
  type=2
)
zones_lasso <- factor(zones_lasso)
table(zones_lasso)
round(table(zones_lasso)/length(zones_lasso)*100, 3)


zones_naive <- getParkesZones(
  referenceVals = test_naive$y12_shifted,
  testVals = test_naive$y12,
  type=2
)
zones_naive <- factor(zones_naive)
table(zones_naive)
round(table(zones_naive)/length(zones_naive)*100, 3)


zones_avg <- getParkesZones(
  referenceVals = avg,
  testVals = y_true,
  type=2
)
zones_avg <- factor(zones_avg)
table(zones_avg)
round(table(zones_avg)/length(zones_avg)*100, 3)
