# Load libraries that are required for the similation
library(tidyverse)
library(ggplot2)

Mining <- data.frame()      # Data Frame, no stress
BestFitDf <- data.frame()   #
BestFitData <- data.frame()
obsN <- 0                   # counter for the data frame that stores the results, do not change
Pvals <- c(1:10)            # The range of P-values I want to achive, keep as 1:10 for 1% - 10% levles
MaxPvalue <- 99         # just to set the stage for the "while" loop later
Ns = c(25) # 50,100,1000,10000)    # Set the number of observatiosn to regress, feel free to change
Runs <- c(1:100)                # Number times you want to "achive" set P-value target

for (N in Ns)       {   # I want to do this for N many observations
  for (j in Runs)     {       # I want to achive a P-value < j, Runs times
    for (i in Pvals)    {     # I want to run through all i p-values

      # I want to run a loop, until P-value < i
      KeyPvalue <- 99   # set minimum P-value to be above minimum 
      Counter <- 0      # This is a counter, that counts how many times I have generated two random series of data
      
      # generate new random series of data until I find a significant p-value 
      # note, I have picked a random distr. with mean = 5 and sd = 2
      while(KeyPvalue > i/100) {   
        Df <-   data.frame(
                  rnorm(n = N, mean = 5,sd = 2), 
                  rnorm(n = N, mean = 5,sd = 2),
                  N
                )
        Df <- setNames(Df, c("Random_1", "Random_2", "N"))   # put names on vars 
        
        # Regress the random series on each other
        Regression <- lm(data = Df, 
           formula = Random_1 ~ Random_2
          )
        
        KeyPvalue <- summary(Regression)$coefficients[2,4]  # Store current p-value
        Counter <- Counter + 1  # count the number of regression attempts
        
        # store the best regression in a Dataframe
        if (KeyPvalue < MaxPvalue) {
          MaxPvalue <- KeyPvalue
          BestFitDf <- Df
        }
        
      }
      
    # populate data frame with the  number of attempts and P-value limit 
    obsN <- obsN + 1
    Mining[obsN,1] <- Counter
    Mining[obsN,2] <- i/100
    Mining[obsN,3] <- N
    
    print(paste0("Nss ", N))  # nice to see where you stand
    }
    
    print(paste0("run n ",j)) # again nice
  }
  
  # put the best regression of each sample size in dataframe
  BestFitData <- rbind(BestFitData, BestFitDf)
  MaxPvalue <- 99   #reset max p-value for findg best regression above
}

# Give varibles names
Mining <-  setNames(Mining, c("Loops", "PValueLimit", "NumberOfObs"))

# scatter different sample sizes
ggplot(data = BestFitData) +
  aes(x = Random_2, y = Random_1) +
  geom_point() + 
  geom_smooth(method = lm) +
  facet_wrap(vars(N)) +
  xlab("Random draw 2 [X~N(5,2)]") +
  ylab("Random draw 1 [X~N(5,2)]")

# single box chart 
ggplot(data = Mining) +
  aes(y = Loops, x = as.character(PValueLimit)) +
  geom_boxplot() + 
  xlab("Indipendent varible P-Value") +
  ylab("Number of attempts (randomlly generated set of numers)")

# summary dataset
SummaryResults <- Mining %>%
  group_by(PValueLimit, NumberOfObs) %>%
  summarise(sum = sum(Loops),
            mean = mean(Loops))

# insert a varible into Summary data, to show that P-value is almost always the same
SummaryResults$Probability <- round(max(Runs) / SummaryResults$sum,digits = 2)

# Plot all the data in Mining, to show how different p-values are reached
Mining %>%
  ggplot() +
    aes(y = Loops, x = as.factor(NumberOfObs)) +
    geom_boxplot() +
    facet_wrap(vars(PValueLimit)) +
    coord_flip()

