#Functions -- Annual Spending Function  

library(Rcpp)
library(ggplot2)
library(reshape2)

# function tpv returns the integer year in which the spending model
# portfolio balance falls to zero. x is the vector containing market return growth
# factors (market return plus one). initportfolio is the initial portfolio balance.
# years is the max number of years to calculate (function returns 99 if balance never
# falls to zero.) spending contains the constant annual spending amount. 
# Start C++ vector index at zero (R starts at 1.)

cppFunction('int tpvf (NumericVector x, int initportfolio, int years, double spending) {
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
print("Print TPV's with Fixed Lifetimes")
print("**********************")

set.seed(27514)
scenarios <- 10000
years <- 30
 
geoMeanC <- rep(0,scenarios)
marketReturns <- matrix(0,nrow=scenarios,ncol=45)
resultC <- rep(0,scenarios)
stdev <- rep(0,scenarios)
spending <- 40000
port <- 1000000
mu <- .05
sigma <- 0.11

r1 <- .025 # top of worst quartile
r2 <- mu # mean
r3 <- .075 # bottom of 4th Quartile

print(paste("mu = ",mu,"sigma =",sigma,"spending =",spending,sep=" "))

for (i in 1:scenarios) {
  marketReturns[i,] <- rlnorm(45,mu,sigma)
  x <- marketReturns[i,1:30]
  geoMeanC[i] <- exp(mean(log(x))) - 1
  resultC[i] <- tpvf(x,port,years,spending)
  stdev[i] <- sd(x)

}
proc.time() - ptm 

scatplot.df <- data.frame(cbind(geoMeanC,resultC))
colnames(scatplot.df) <- c("Return","TPV")

scatplot.df[["sign"]] = ifelse(scatplot.df[["TPV"]] < 0, "Ruin",ifelse(scatplot.df[["TPV"]] < port, "CondSuccess","Success"))
scatplot.df <- melt(scatplot.df,id.vars = c("Return","TPV"))
scatplot.df[,3] <- NULL
colnames(scatplot.df) <- c("Return","values","Sign")
options (scipen = 8)

p <- ggplot(scatplot.df, aes(Return*100,values/1000000,fill=Sign)) +
  scale_color_manual(values = c("Success" = "blue3","CondSuccess"="blue1", "Ruin" = "red")) +
  geom_point(aes(color = Sign),size=1) +
  xlab("Percent Geometric Mean Annual Return") + ylab("Terminal Portfolio Value ($M)") +
  scale_linetype_discrete(name = "") +
  theme_gray() +
  xlim(c(0,10)) +
  ylim(c(-1,10.7)) +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  theme(legend.title=element_blank()) + 
  theme(legend.position='none') +
  ggtitle(paste("Figure 1. Terminal Portfolio Values Assuming\n30-year Retirement, ",spending/port*100,"% Withdrawals",sep="") )
print(p)
 
print("Cumulative Geometric Return Distribution ******")
print(paste("Prob return <",round(r1,3)," is:",sizeQ1/scenarios*100,"%","             Prob of Ruin is: ",round(ruinQ1/sizeQ1*100,1),"%"),sep="")
print(paste("Prob return ",round(r1,3)," to ",round(r2,3)," is:",sizeQ2/scenarios*100,"%","    Prob of Ruin is: ",round(ruinQ2/sizeQ2*100,1),"%"),sep="")
print(paste("Prob return ",round(r2,3)," to ",round(r3,3)," is:",sizeQ3/scenarios*100,"%","    Prob of Ruin is: ",round(ruinQ3/sizeQ3*100,1),"%"),sep="")
print(paste("Prob return >",round(r3,3)," is:",sizeQ4/scenarios*100,"%","             Prob of Ruin is: ",round(ruinQ4/sizeQ4*100,1),"%"),sep="")
print(" ")
print(" ")

mtpvf <- median(resultC[resultC>0])

print(paste("Median successful TPV is: ",round(mtpvf,0),sep=""))

fruin <- sum(resultC<=0)/scenarios

print(paste("Probability of Ruin is:",fruin*100,"%"),sep="")


failedQ1 = sum(scatplot.df$Return<= r1 & scatplot.df$values <= 0)
failedQ4 =  sum(scatplot.df$Return >= r3 & scatplot.df$values <= 0)
failedQ3 =  sum(scatplot.df$Return>=r2 & scatplot.df$Return<r3 & scatplot.df$values <= 0)
failedQ2 =  sum(scatplot.df$Return>=r1 & scatplot.df$Return<r2 & scatplot.df$values <= 0)

sizeQ1 <- sum(scatplot.df$Return < r1) # count number of outcomes in this range of returns
sizeQ2 <- sum(scatplot.df$Return >=r1 & scatplot.df$Return < r2)
sizeQ3 <- sum(scatplot.df$Return >=r2 & scatplot.df$Return < r3)
sizeQ4 <- sum(scatplot.df$Return >=r3)

print(paste("Q1 % Ruined is: ",round(failedQ1/sizeQ1,2)*100,"%",sep=""))
print(paste("Q2 % Ruined is: ",round(failedQ2/sizeQ2,2)*100,"%",sep=""))
print(paste("Q3 % Ruined is: ",round(failedQ3/sizeQ3,2)*100,"%",sep=""))
print(paste("Q4 % Ruined is: ",round(failedQ4/sizeQ4,2)*100,"%",sep=""))

print(paste("Q1 Size is: ",sizeQ1,sep=""))
print(paste("Q2 Size is: ",sizeQ2,sep=""))
print(paste("Q3 Size is: ",sizeQ3,sep=""))
print(paste("Q4 Size is: ",sizeQ4,sep=""))
print(paste("Total is ",sizeQ1+sizeQ2+sizeQ3+sizeQ4,sep=""))

mtpvQ1 =  median(scatplot.df$values[scatplot.df$Return<r1 & scatplot.df$values>0])
mtpvQ4 =  median(scatplot.df$values[scatplot.df$Return>=r3 & scatplot.df$values>0])
mtpvQ3 =  median(scatplot.df$values[scatplot.df$Return>=r2 & scatplot.df$Return<r3 & scatplot.df$values>0])
mtpvQ2 =  median(scatplot.df$values[scatplot.df$Return>=r1 & scatplot.df$Return<r2 & scatplot.df$values>0])

print(paste("Q1 median successful TPV is: ",round(mtpvQ1,0),sep=""))
print(paste("Q2 median successful TPV is: ",round(mtpvQ2,0),sep=""))
print(paste("Q3 median successful TPV is: ",round(mtpvQ3,0),sep=""))
print(paste("Q4 median successful TPV is: ",round(mtpvQ4,0),sep=""))

meanretQ1 <- mean(scatplot.df$Return[scatplot.df$Return<r1])
meanretQ2 <- mean(scatplot.df$Return[scatplot.df$Return>=r1 & scatplot.df$Return<r2])
meanretQ3 <- mean(scatplot.df$Return[scatplot.df$Return>=r2 & scatplot.df$Return<=r3])
meanretQ4 <- mean(scatplot.df$Return[scatplot.df$Return>=r3])

print(paste("Q1 Mean Annual Return is: ",round(meanretQ1*100,2),"%",sep=""))
print(paste("Q2 Mean Annual Return is: ",round(meanretQ2*100,2),"%",sep=""))
print(paste("Q3 Mean Annual Return is: ",round(meanretQ3*100,2),"%",sep=""))
print(paste("Q4 Mean Annual Return is: ",round(meanretQ4*100,2),"%",sep=""))

ggsave("~/desktop/Figure1.png",dpi=600)

#dev.off()
