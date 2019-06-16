library(ggplot2)

g1 <- read.csv("Dataset-G1.csv", sep = ';')

vq <- as.numeric(g1$VQ)
br <- g1$Bitrate

# graph <- ggplot(g1, aes(br, vq)) +
#   geom_point()
  # geom_smooth(method = "lm", se = F)

# plot(log10(br), log10(vq))
# boxplot(log10(br))

incelenecekler = array(c(br, "Bitrate", "Bitrate - Video Quality Scatter Plot"))

smooth_scattering(br, vq, incelenecekler[2001], main_title = incelenecekler[2002])
lin_modelling(br, vq, incelenecekler[2001])
dist_comp(vq, br, incelenecekler[2001])

#scatter.smooth(br, vq, xlab = "Bitrate", ylab = "Video Quality", main = "Bitrate - Video Quality Scatter Plot"  ,col='orange', lwd=1, lty=3)

# par(mfrow=c(2, 1))
# plot(density(vq), main = "Video Quality Distribution")
# polygon(density(vq), col = 'yellow')
# 
# plot(density(br), main = "Bitrate Distribution")
# polygon(density(br), col = 'red')

# sink('./VQ-BR-Linear-Model.txt')
# lin_mod <- lm(vq ~ br)
# summary(lin_mod)
# sink()


smooth_scattering <- function(v1, v2, x_side, y_side="Video Quality", main_title) {
  
  scatter.smooth(v1, v2, xlab = x_side, ylab = y_side, main = main_title, col = "orange", lwd=1, lty=3)
  
}

lin_modelling <- function(v1, v2, v1_name, v2_name="Video Quality") {
  
  sink(paste('./', v1_name, '-', v2_name, '-Linear-Model.txt'), append = F)
  # sink('./Linear-Model.txt', append = T)
  lmod <- lm(v1~v2)
  (summary(lmod))
  # sink()
  
}

dist_comp <- function(v1, v2, dist1_name, dist2_name="Video Quality") {
  par(mfrow=c(2, 1))
  plot(density(v1), main = paste(dist1_name, " Distribution"))
  polygon(density(v1), col = 'yellow')
  
  plot(density(v2), main = paste(dist2_name, " Distribution"))
  polygon(density(v2), col = 'red')
  
}








