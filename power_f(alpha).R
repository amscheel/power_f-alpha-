##########################################################################################
# This script will plot power for a t-test of a single effect size (d) for
# different sample sizes (N) as a function of alpha. This was inspired by the discussion
# about whether the convention for alpha should be lowered from .05 to .005.
##########################################################################################

#-----------------------------------------------------------------------------------#
# install and load required packages

if(!require(pwr)){
  install.packages("pwr")
}
if(!require(reshape2)){
  install.packages("reshape2")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
library(pwr)
library(reshape2)
library(ggplot2)
#-----------------------------------------------------------------------------------#

#####################################################################
# (1) SET VALUES: effect size (Cohen's d), total sample size (N),
# upper bound of the alpha range you're interested in, and
# decide if you want to look at two-sample t-tests or paired t-tests
#####################################################################
d <- 0.5  # set effect size (Cohen's d for a two-sample t-test)
N <- c(40, 60, 100, 150, 200)  # define N you're interested in (change or add values)
upper.alpha <- 0.1  # set upper bound of the alpha range you're interested in
type <- "two.sample" # for a paired-sample t-test, change this to "paired"
#-----------------------------------------------------------------------------------#

#############################################
# (2) Do the maths: Run code down to line 56 
#############################################
# create continuous vector for alpha
alpha <- c((1:(upper.alpha*10000))/10000) 

# create a matrix to fill with values
powermat <- matrix(data=NA, nrow=length(alpha), ncol=length(N)+1)
colnames(powermat) <- c("alpha", N)  #rename columns of mat
powermat[,1] <- alpha

# for loop to calculate power for different N and levels of alpha
for(i in 1:length(N)){ 
  b <- pwr.t.test(d=d, n=N[i], sig.level=alpha, type=type, alternative="two.sided")
  powermat[,i+1] <- b$power
}

# Turn the matrix into a data frame
powermat <- as.data.frame(powermat)
# Turn the four data frame into long format
powermat <- melt(powermat, id.vars="alpha", variable.name="N", value.name = "power")
#-----------------------------------------------------------------------------------#


#####################################################################################
# (3) PLOTTING
#####################################################################################
cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#F0E442", "#0072B2", "#999999")

# draw vertical lines to emphasise alpha levels of interest
# change the following three values to the ones you're interested in
alpha1 <- 0.05  # conventional alpha level 
alpha2 <- 0.005 # alpha level suggestion by Benjamin et al. (2017), https://osf.io/preprints/psyarxiv/mky9j/
alpha3 <- 0.025

# make the plot
powerplot <- ggplot(powermat, aes(x=powermat$alpha, y=powermat$power, colour=powermat$N))  +  
  geom_line() +
  #geom_hline(aes(yintercept=0.95), colour="#999999") +
  geom_vline(aes(xintercept=alpha1), colour="#999999") +
  geom_vline(aes(xintercept=alpha2), colour="#999999") +
  geom_vline(aes(xintercept=alpha3), colour="#999999") +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(face="bold")) +
  ggtitle(paste("Power for a ", ifelse(type == "two.sample", "two-sample", "paired"), " t-test of d = ", d, sep="")) +
  scale_y_continuous(name="power", limits=c(0, 1), breaks=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1)) +
  scale_x_continuous(name="alpha", breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)) +
  annotate("text", x=alpha1 - 0.0005, y=0.01, label = alpha1, colour="darkgrey", hjust = 1) +
  annotate("text", x=alpha2 - 0.0005, y=0.01, label = alpha2, colour="darkgrey", hjust = 1) +
  annotate("text", x=alpha3 - 0.0005, y=0.01, label = alpha3, colour="darkgrey", hjust = 1) +
  scale_colour_discrete(name="N")
# If you prefer a colourblind-friendly palette, replace the last argument with
#  scale_colour_manual(name="N", values=cbPalette) 

#show the plot
powerplot
#-----------------------------------------------------------------------------------#