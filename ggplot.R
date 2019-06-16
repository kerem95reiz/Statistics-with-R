library(ggplot2)
library(dslabs)
library(dplyr)
library(ggthemes)
library(ggrepel)
data(murders)


# geometri eklemedegimiz icin, yalnizca gri arka plan
# ggplot(murders) +
#  geom_point(aes(murders$population/10^6, murders$total), size=3) +
#  geom_text(aes(murders$population/10^6, murders$total, label = murders$abb), nudge_x = 2)
  

# layerlar geometri tanimi, istatistik hesabi, style degisimi icin kullanilan katmanlar
#DATA %>% ggplot() + Layer1 + Layer2...

# ortalam olum orani
r <- murders %>% summarise(rate = sum(total) / sum(population)*10^6) %>% .$rate

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + 
  theme_economist() +
  geom_abline(intercept =log10(r), lty=2, color="darkgrey" ) +
  geom_point(aes(col = murders$region), size=3) +
  geom_text_repel(nudge_x = 0.075) +
  scale_x_continuous(trans = "log10") +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total murders (log scale)") +
  ggtitle("Gun Murders")
