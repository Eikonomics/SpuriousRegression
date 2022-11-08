library(tidyverse)
library(ggplot2)
Mining <- data.frame()  # Data Frame, no stress
MaxPvalue <- 99         # just to set the stage, do not change
obsN <- 0               # counter for data frame, do not change
Pvals <- c(1:10)        # keep as 1:10

Ns = 100                 # Set the number of observatiosn to regress, feel free to change
Runs <- c(1:100)         # Number times you want to "achive" set P-value target


for (j in Runs) {
  
  for (i in Pvals) {
    #print(i)  
    KeyPvalue <- 99
    Counter <- 0
    
    while(KeyPvalue > i/100) {
      Df <-   data.frame( 
        runif(n = Ns, min = 0,max = 10),
        runif(n = Ns, min = 0,max = 10)
      )
      Df <- setNames(Df, c("Random_1", "Random_2"))
      
      Regression <- lm(data = Df, 
                       formula = Random_1 ~ Random_2
      )
      
      KeyPvalue <- summary(Regression)$coefficients[2,4]
      Counter <- Counter + 1
      
      if (KeyPvalue < MaxPvalue) {
        MaxPvalue <- KeyPvalue
        BestFitDf <- Df  
      }
      
    }
    obsN <- obsN + 1
    print(Counter)
    Mining[obsN,1] <- Counter
    Mining[obsN,2] <- i/100
    
  }
}
Mining <-  setNames(Mining, c("Loops", "PValueLimit"))
ggplot(data = BestFitDf) +
  aes(x = Random_2, y = Random_1) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlab("Random draw 2 [X~N(5,2)]") +
  ylab("Random draw 1 [X~N(5,2)]")

UnifromDistAssSumm <- summary(lm(data = BestFitDf, Random_1 ~ Random_2))

ggplot(data = Mining) +
  aes(y = Loops, x = as.character(PValueLimit)) +
  geom_boxplot() + 
  xlab("Indipendent varible P-Value") +
  ylab("Number of attempts (randomlly generated set of numers)")


SummaryResults <- Mining %>%
                    group_by(PValueLimit) %>%
                    summarise(sum = sum(Loops),
                              mean = mean(Loops))
SummaryResults$Probability <- 100 / SummaryResults$sum

