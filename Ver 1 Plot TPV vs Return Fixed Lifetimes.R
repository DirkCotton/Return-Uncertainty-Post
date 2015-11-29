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
resultC <- rep(0,scenarios)
stdev <- rep(0,scenarios)
spending <- 40000
port <- 1000000
mu <- .05
sigma <- 0.11

ptm <- proc.time()

for (i in 1:scenarios) {
  x <- rlnorm(years,mu,sigma)
  if (i==1306) xx <- x
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
  ylim(c(-1,7.6)) +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  theme(legend.title=element_blank()) + 
  theme(legend.position='none') +
  ggtitle(paste("Terminal Portfolio Values Assuming\n30-year Retirement, ",spending/port*100,"% Withdrawals",sep="") )
print(p)
 
print("Cumulative Geometric Return Distribution ******")
print(paste(">= 7.5% is: ",100*sum(geoMeanC>.075)/scenarios,"%",sep=""))
print(paste("5% to 7.5% is: ",100*(sum(geoMeanC>.05)/scenarios - sum(geoMeanC>.075)/scenarios),"%",sep=""))
print(paste("2.5% to 5% is: ",100*(sum(geoMeanC>=.025)/scenarios - sum(geoMeanC>.05)/scenarios),"%",sep=""))
print(paste("<= 2.5 is: ",100*sum(geoMeanC<.025)/scenarios,"%",sep=""))
mtpvf <- mean(resultC)

print(paste("Mean TPV is: ",round(mtpvf,0),sep=""))

fruin <- sum(resultC<=0)/scenarios

print(paste("Probability of Ruin is:",fruin*100,"%"),sep="")

mtpvQ1 =  mean(scatplot.df$values[scatplot.df$Return<.025])
mtpvQ4 =  mean(scatplot.df$values[scatplot.df$Return>=.075])
mtpvQ3 =  mean(scatplot.df$values[scatplot.df$Return>=.05 & scatplot.df$Return<.075])
mtpvQ2 =  mean(scatplot.df$values[scatplot.df$Return>=.025 & scatplot.df$Return<.05])

print(paste("Q1 mean TPV is: ",round(mtpvQ1,0),sep=""))
print(paste("Q2 mean TPV is: ",round(mtpvQ2,0),sep=""))
print(paste("Q3 mean TPV is: ",round(mtpvQ3,0),sep=""))
print(paste("Q4 mean TPV is: ",round(mtpvQ4,0),sep=""))

#dev.off()
