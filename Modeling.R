library(ggplot2)

g1 <- read.csv("Dataset-G1.csv", sep = ';')

vq <- as.numeric(g1$VQ)
br <- g1$Bitrate
res <- g1$Resolution
fr <- g1$Framerate

incelenecekler = array(c(br, "Bitrate", "Bitrate - Video Quality Scatter Plot"))
res_array = array(c(res, "Resolution", "Resolution - Video Quality Scatter Plot"))
fr_array <- array(c(fr, "Framerate", "Framerate - Video Quality Scatter Plot"))

# Bitrate Icin
smooth_scattering(br, vq, incelenecekler[2001], main_title = incelenecekler[2002])
# lin_modelling(br, vq, incelenecekler[2001])
dist_comp(br, vq, incelenecekler[2001])

# Resolution icin
smooth_scattering(res, vq, res_array[2001], main_title = res_array[2002])
# lin_modelling(res, vq, res_array[2001])
dist_comp(res, vq, res_array[2001])

#Framerate
smooth_scattering(fr, vq, fr_array[2001], main_title = fr_array[2002])
# lin_modelling(fr, vq, fr_array[2001])
dist_comp(fr, vq, fr_array[2001])

# Burda iki degiskenin scatter graphi olusturuluyor
# v1: Independent Variable
# v2: Dependent Variable
# x_side: name of the independent var
# y_side: name of the dependent var
# main_title: name the graph
smooth_scattering <- function(v1, v2, x_side, y_side="Video Quality", main_title) {
  scatter.smooth(v1, v2, xlab = x_side, ylab = y_side, main = main_title, col = "orange", lwd=1, lty=3)
}

# Burda biri bagimli digeri bagimsiz iki degiskene ait lineer model olusturuluyor
# v1 bagimsiz degisken
# v2 bagimli degisken
# v1_name bagimsiz degiskenin ismi
# v2_name bagimli degiskenin ismi
lin_modelling <- function(v1, v2, v1_name, v2_name="Video Quality") {
  sink(paste('./', v1_name, '-', v2_name, '-Linear-Model.txt'), append = F)
  lmod <- lm(v1~v2)
  print(summary(lmod))
  sink()
}

# Iki degiskenin dagilimlari karsilastiriliyor
# v1 bagimsiz degisken
# v2 bagimli degisken
# v1_name bagimsiz degiskenin ismi
# v2_name bagimli degiskenin ismi
dist_comp <- function(v1, v2, v1_name, v2_name="Video Quality") {
  par(mfrow=c(2, 1))
  plot(density(v1), main = paste(v1_name, " Distribution"))
  polygon(density(v1), col = 'yellow')
  
  plot(density(v2), main = paste(v2_name, " Distribution"))
  polygon(density(v2), col = 'red')
}








