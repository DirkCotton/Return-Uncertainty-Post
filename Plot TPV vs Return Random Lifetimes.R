#Functions -- Annual Spending Function   

# Run fixed lifetime script first!
  
library(Rcpp) 
library(ggplot2)
library(reshape2)

# function tpv returns the integer year in which the spending model
# portfolio balance falls to zero. x is the vector containing market return growth
# factors (market return plus one). initportfolio is the initial portfolio balance.
# years is the max number of years to calculate (function returns 99 if balance never
# falls to zero.) spending contains the constant annual spending amount. 
# Start C++ vector index at zero (R starts at 1.)

cppFunction('int tpv (NumericVector x, int initportfolio, int years, double spending) {
  int balance = initportfolio;
  int k;
  
  for (k = 0; k <= years-1; ++k) {
    balance = (balance - spending); 
    if (balance >= 0) balance = balance * x[k];
  }
  return balance;
}'
)

print (" ")
print (" ")
print("**********************")
print("Print TPV's with Random Lifetimes")
print("**********************")

# Working directories (set to "" to use current working directory)
modelWD <- "/Users/dirkcotton/CourseraR/Retirement 2/"
figureWD <- "~/Desktop/R/Return Uncertainty Post/" # directory to store all plots generated

# Initialize variables 

set.seed(27514)
scenarios <- 10000
years <- max(lifeSpanJoint$V1)
geoMeanC <- rep(0,scenarios)
resultC <- rep(0,scenarios)
stdev <- rep(0,scenarios)
spending <- 40000
port <- 1000000
mu <- .05
sigma <- 0.11

# Build random lifetimes

ptm <- proc.time()

for (i in 1:scenarios) {
  #x <- rlnorm(years,mu,sigma)
  x <- marketReturns[i,1:lifeSpanJoint$V1[i]]
  geoMeanC[i] <- exp(mean(log(x))) - 1
  resultC[i] <- tpv(x,port,lifeSpanJoint$V1[i],spending)
  stdev[i] <- sd(x)
} 
proc.time() - ptm 

scatplot.df <- data.frame(cbind(geoMeanC,resultC))
colnames(scatplot.df) <- c("Return","TPV")
scatplot.df[["sign"]] = ifelse(scatplot.df[["TPV"]] < 0, "Ruin",ifelse(scatplot.df[["TPV"]] < port, "CondSuccess","Success"))
scatplot.df <- melt(scatplot.df,id.vars = c("Return","TPV"))
scatplot.df[,3] <- NULL
colnames(scatplot.df) <- c("Return","values","Sign")

options (scipen = 8) # change x-axis from scientific notation to integer

p <- ggplot(scatplot.df, aes(Return*100,values/1000000,fill=Sign)) +
  scale_color_manual(values = c("Success" = "blue3","CondSuccess"="blue1", "Ruin" = "red")) +
  geom_point(aes(color = Sign),size=1) +
  xlab("Percent Geometric Mean Annual Return") + ylab("Terminal Portfolio Value ($M)") +
  scale_linetype_discrete(name = "") +
  theme_gray() +
  xlim(c(0,10)) +
  ylim(c(-1,10)) +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  theme(legend.title=element_blank()) + 
  theme(legend.position='none') +
  geom_vline(xintercept=mu*100) + geom_hline(yintercept=0) +
  ggtitle(paste("Terminal Portfolio Values Assuming\nJoint Life Expectancy, ",spending/port*100,"% Withdrawals",sep="") )
 print(p)
 
mtpvr <- mean(resultC)
print(paste("Mean TPV is: ",round(mtpvr,0)),sep="")
print(paste("Mean TPV is",round(((mtpvr-mtpvf)/mtpvf)*100,1)," % lower than for fixed lifetimes."),sep="")

rruin <- sum(resultC<=0)/scenarios

print(paste("Probability of Ruin is:",rruin*100,"%"),sep="")

print(paste("Prob Ruin for Random lifetimes is",round(((rruin-fruin)/fruin)*100,1)," % lower than for fixed lifetimes."),sep="")

q2 <- subset(scatplot.df,Return<.05 & Return >=.025 & values <=0)
q3 <- subset(scatplot.df,Return<.075 & Return >=.05 & values <=0)
q4 <- subset(scatplot.df,Return >=.075 & values <=0)
q1 <- subset(scatplot.df,Return<.025 & values <=0)

ruinQ1 <- length(q1$Return) # count ruined portfolios
ruinQ2 <- length(q2$Return)
ruinQ3 <- length(q3$Return)
ruinQ4 <- length(q4$Return)

sizeQ1 <- sum(scatplot.df$Return < .025) # count number of outcomes in this range of returns
sizeQ2 <- sum(scatplot.df$Return >=.025 & scatplot.df$Return < .05)
sizeQ3 <- sum(scatplot.df$Return >=.05 & scatplot.df$Return < .075)
sizeQ4 <- sum(scatplot.df$Return >=.075)

mtpvQ1 =  median(scatplot.df$values[scatplot.df$Return<.025 & scatplot.df$values>0])
mtpvQ4 =  median(scatplot.df$values[scatplot.df$Return>=.075 & scatplot.df$values>0])
mtpvQ3 =  median(scatplot.df$values[scatplot.df$Return>=.05 & scatplot.df$Return<.075 & scatplot.df$values>0])
mtpvQ2 =  median(scatplot.df$values[scatplot.df$Return>=.025 & scatplot.df$Return<.05 & scatplot.df$values>0])



print(paste("Distribution of Geometric Mean Returns**************"),sep="")
print(paste("Prob return < .025 is:",sizeQ1/scenarios*100,"%","Prob of Ruin is: ",round(ruinQ1/sizeQ1*100,1),"%"),sep="")
print(paste("Prob return .025 to .05 is:",sizeQ2/scenarios*100,"%","Prob of Ruin is: ",round(ruinQ2/sizeQ2*100,1),"%"),sep="")
print(paste("Prob return .05 to .075 is:",sizeQ3/scenarios*100,"%","Prob of Ruin is: ",round(ruinQ3/sizeQ3*100,1),"%"),sep="")
print(paste("Prob return >.075 is:",sizeQ4/scenarios*100,"%","Prob of Ruin is: ",round(ruinQ4/sizeQ4*100,1),"%"),sep="")

print(paste("Q1 median successful TPV is: ",round(mtpvQ1,0),sep=""))
print(paste("Q2 median successful TPV is: ",round(mtpvQ2,0),sep=""))
print(paste("Q3 median successful TPV is: ",round(mtpvQ3,0),sep=""))
print(paste("Q4 median successful TPV is: ",round(mtpvQ4,0),sep=""))

# Plot age against TPV

atpv.df <- data.frame(cbind(lifeSpanJoint$V1+65,resultC))
colnames(atpv.df) <- c("Age","TPV")

p2 <- ggplot(atpv.df, aes(Age,TPV/1000000)) +
  # scale_color_manual(values = c("Success" = "blue3","CondSuccess"="blue1", "Ruin" = "red")) +
  geom_point(size=1) +
  xlab("Age") + ylab("Terminal Portfolio Value ($M)") +
  scale_linetype_discrete(name = "") +
  xlim(c(65,100)) +
  ylim(c(-1,10)) +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  #theme(legend.position='none') +
  #geom_vline(xintercept=mu*100) + geom_hline(yintercept=0) +
  ggtitle("Terminal Portfolio Values versus Age at Death")
#print(p2)

failedQ1 = sum(scatplot.df$Return<= .025 & scatplot.df$values <= 0)
failedQ4 =  sum(scatplot.df$Return >= .075 & scatplot.df$values <= 0)
failedQ3 =  sum(scatplot.df$Return>=.05 & scatplot.df$Return<.075 & scatplot.df$values <= 0)
failedQ2 =  sum(scatplot.df$Return>=.025 & scatplot.df$Return<.05 & scatplot.df$values <= 0)

print(paste("Q1 % Ruined is: ",round(failedQ1/sizeQ1,2)*100,"%",sep=""))
print(paste("Q2 % Ruined is: ",round(failedQ2/sizeQ2,2)*100,"%",sep=""))
print(paste("Q3 % Ruined is: ",round(failedQ3/sizeQ3,2)*100,"%",sep=""))
print(paste("Q4 % Ruined is: ",round(failedQ4/sizeQ4,2)*100,"%",sep=""))

print(paste("Q1 Size is: ",sizeQ1,sep=""))
print(paste("Q2 Size is: ",sizeQ2,sep=""))
print(paste("Q3 Size is: ",sizeQ3,sep=""))
print(paste("Q4 Size is: ",sizeQ4,sep=""))

print(paste("Total is ",sizeQ1+sizeQ2+sizeQ3+sizeQ4,sep=""))