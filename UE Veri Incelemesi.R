library(ggplot2)
library(e1071)

ds <- read.csv2("./Dataset-G1.csv")

vq <- ds$VQ
fr <- ds$VF

hist(vq, xlab = "Quality of Video", freq = FALSE, col = blues9)
hist(fr, xlab = "Fragmentation of Video", col = blues9)
plot(fr, vq, main = "Fragmentation vs Video Quality Relation", xlab = "Fragmentation", ylab = "Video Quality")
ggplot() + geom_point(aes(fr, vq))

#qqnorm(vq)
qqline(vq)

# scatter graf ciz ve verinin trendini bul. Veriler arasinda ki iliskiyi daha iyi anlamak icin. 
scatter.smooth(fr, vq, xlab = "Fragmentation of Video", ylab = "Quality of Video", main= "Fragmentation ve Quality Iliskisi")

# Outlier var mi yokmu kontrol et
boxplot(fr, xlab = "Fragmentation of Video", sub=paste("Outlier ",boxplot.stats(fr)$out))
boxplot(vq, xlab = "Quality of Video")

# Verilerin dagilimini kontrol et
polygon(density(vq), col="red",sub = paste("Skewness: ", round(e1071::skewness(vq), 2)))

# Son olarak linear model su sekilde olusturulabilir
# vq_fr <- lm(vq~fr, data.frame(c(vq, fr)))
vq_fr <- lm(vq ~ fr)
summary(vq_fr)
















