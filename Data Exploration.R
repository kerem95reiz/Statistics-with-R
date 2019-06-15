library(tidyverse)
library(dplyr)


#(Dataset_G1.PID)
x <- read.csv2("Dataset-G1.csv")

# frag <- x['VF']
# importance_frag <- x['IVF']

# class(frag) # bu data frame, cunku alt kume 
# class(importance_frag) # bu ise vektor

# frag_and_imp <- c(frag, importance_frag)
# frag_and_imp <- data.frame(frag, importance_frag)
frag_and_imp <- select(x, VF, IVF)
# plot(frag_and_imp$VF, frag_and_imp$IVF)
boxplot(VF~IVF, frag_and_imp)

freq <- dplyr::select(x, VF)
p <- seq(0.05, 0.95, by=0.05)
observed_quantiles <- quantile(x$VF, p)
theortical_quantiles <- qnorm(p, mean = mean(x$VF), sd = sd(x$VF))
plot(theortical_quantiles, observed_quantiles)
abline(0, 1)






