#!/usr/bin/env Rscript
require(caTools)
require(ggplot2)
require(gridExtra)

dataset <- read.csv('Dataset-G1.csv', sep = ';', stringsAsFactors = F, dec = ',')

## Functions
rsq <- function(x, y) cor(x, y)^2
rmse <- function(x, y) sqrt(mean((x-y)^2))

create_dens <- function(variab, ds) {
  dens <- ggplot(ds, aes_string(x=variab))+  
    geom_histogram(aes(y=..density..), position = 'identity', alpha=1, color='white', fill='white') + 
    geom_density(alpha=.8, fill='red') +
    geom_vline(aes(xintercept=mean(ds[,variab])), color='blue', lty=2)+
    scale_color_brewer(palette = 'Dark2') +
    theme_black()
  print(dens)
}

 save_the_plot <- function(graph, graph_name) {
  ggsave(paste('~/Desktop/',graph_name,'.png', sep = ''), plot = graph, width = 16, height = 9, dpi = 300, device = 'png')  
}

 vals <- function(val, val_name, intervalls) {
   ind_vars <- ggplot()+
     geom_point(aes(x=seq(1, length(val)), 
                    y=val, 
                    colour=cut(val, intervalls)
     ))+
     theme(legend.position = 'none')+
     labs(title=paste('Values Of ', val_name, sep=''), x="Order In The List", y=val_name, colour="Intervalls")+
     theme_black()
 }
 
 
 # Create the plots for the indvidul metrics
 metric_comp_plot <- function(dir_indir, type_of_it="All"){
   print(typeof(dir_indir))
   ggplot()+
     geom_bar(aes(x=dir_indir$Type,
                  y=dir_indir$Val,
                  fill=dir_indir$Dir_Indir,
                  group=dir_indir$Dir_Indir), 
              stat='identity',
              position = position_dodge(),
              color='black'
     )+
     theme_black() +
     labs(title = paste(type_of_it, "Comparison") , x=NULL, y=NULL , fill="Techniques")+
     scale_fill_manual(values = c('white', 'red'))
 }
 
 
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

# Split the Data Set into train and test parts
set.seed(2019) # seed for the random number generator
sampl <- sample.split(dataset$PID, SplitRatio = 0.8)
train <- subset(dataset, sampl == TRUE)
test <- subset(dataset, sampl == FALSE)

# Independent Variables from train data set
br <- train$Bitrate
fr <- train$Framerate
res <- train$Resolution

# Dependent Variables from train data set
vq <- train$VQ
frag <- train$VF
disc <- train$VD
uncl <- train$VU

# Independent Variables from test data set
br_test <- test$Bitrate
fr_test <- test$Framerate
res_test <- test$Resolution

# Dependent Variables from test data set
vq_test <- test$VQ
frag_test <- test$VF
disc_test <- test$VD
uncl_test <- test$VU

# Video Quality model with independent variables BR, FR and Res
vid_qual_br_fr_res <- lm(vq ~ br + fr + res)
# sink('~/Desktop/vid_qual_with_indep_vars.txt')
# print(summary(vid_qual_br_fr_res))
# sink()

## Video Quality dimensions models with independent variables
# Fragmentation with independent variables
frag_br_fr_res <- lm(frag ~ br + fr + res)
# sink('~/Desktop/frag_with_indep_vars.txt')
# print(summary(frag_br_fr_res))
# sink()

# Discontinuity with independent variables
disc_br_fr_res <- lm(disc ~ br + fr + res)
# sink('~/Desktop/disc_with_indep_vars.txt')
# print(summary(disc_br_fr_res))
# sink()

# Unclearness with independent variables
uncl_br_fr_res <- lm(uncl ~ br + fr + res)
# sink('~/Desktop/uncl_with_indep_vars.txt')
# print(summary(uncl_br_fr_res))
# sink()

# Video Quality model with quality dimension models
vid_qual_frag_disc_uncl <- lm(vq ~ frag + disc + uncl)
# sink('~/Desktop/vid_qual_with_dep_vars.txt')
# print(summary(vid_qual_frag_disc_uncl))
# sink()

# Here is created the model with indirect method. The input is the output of the previous models.
vid_qual_with_predicted_frag_disc_uncl <- lm(vq ~ predicted_frag + predicted_disc + predicted_uncl)
# sink('~/Desktop/vid_qual_with_predicted_frag_disc_uncl.txt')
# print(summary(vid_qual_with_predicted_frag_disc_uncl))
# sink()


# Video Quality model with quality dimension models predicted from independent vars
br_fr_res <- data.frame(br=br, fr=fr, res=res) # Create the data frame to use in the predict function
predicted_frag <- predict.lm(frag_br_fr_res, br_fr_res)
predicted_disc <- predict.lm(disc_br_fr_res, br_fr_res)
predicted_uncl <- predict.lm(uncl_br_fr_res, br_fr_res)

## Get the result of the comparison between test and predicted data
br_fr_res_test <- data.frame(br=br_test, fr=fr_test, res=res_test)
frag_disc_uncl_test <- data.frame(frag=frag_test, disc=disc_test, uncl=uncl_test)

# Video Quality Model that is created with independent vars
pred_vid_qual_with_ind_vars <- predict(vid_qual_br_fr_res, br_fr_res_test)
comp_ind <- data.frame(cbind(actuals=vq_test, predicteds=pred_vid_qual_with_ind_vars))
cor_ind <- cor(comp_ind$actuals, comp_ind$predicteds)
rsq_ind <- rsq(comp_ind$actuals, comp_ind$predicteds)
rmse_ind <- rmse(comp_ind$actuals, comp_ind$predicteds)

# Video Quality Model that is created with hybrid technique
pred_vid_qual_with_both <- predict(vid_qual_with_predicted_frag_disc_uncl,  data.frame(predicted_frag=frag_disc_uncl_test$frag, predicted_disc=frag_disc_uncl_test$disc, predicted_uncl=frag_disc_uncl_test$uncl))
comp_both <- data.frame(cbind(actuals=vq_test, predicteds=pred_vid_qual_with_both))
cor_both <- cor(comp_both$actuals, comp_both$predicteds)
rsq_both <- rsq(comp_both$actuals, comp_both$predicteds)
rmse_both <-rmse(comp_both$actuals, comp_both$predicteds)


# VISUALISING THE DATA
col.1 <- c("Direct", "Direct", "Direct", "Indirect", "Indirect", "Indirect")
col.2 <- c(cor_ind, rsq_ind, rmse_ind, cor_both, rsq_both, rmse_both)
col.3 <- c("Correlation", "RSQ", "RMSE", "Correlation", "RSQ", "RMSE")

metrics <-  matrix(c(col.1,round(col.2, 2), col.3), nrow = 6, ncol = 3) 
colnames(metrics) <- c("Dir_Indir", "Val", "Type")
metrics <- data.frame(metrics)
metric_types <- c("Correlation", "RMSE", "RSQ")

# Each individula metric seperately and then all together
corr_plot <- metric_comp_plot(metrics[which(metrics$Type==metric_types[1]),], metric_types[1])
rmse_plot <- metric_comp_plot(metrics[which(metrics$Type==metric_types[2]),], metric_types[2])
rsq_plot <- metric_comp_plot(metrics[which(metrics$Type==metric_types[3]),], metric_types[3])
bp <- metric_comp_plot(metrics)


# Create density distributions of the independent vars
vq_dens <- create_dens('VQ', dataset)

frag_dens <- create_dens('VF', dataset)
disc_dens <- create_dens('VD', dataset)
uncl_dens <- create_dens('VU', dataset)

br_dens <- create_dens('Bitrate', dataset)
fr_dens <- create_dens('Framerate', dataset)
res_dens <- create_dens('Resolution', dataset)

vq_dims <- grid.arrange(frag_dens, disc_dens, uncl_dens)
ind_vars <- grid.arrange(br_dens, fr_dens, res_dens)


## Showing independent variables for the video quality
# Showing the bitrate values for the list order
br_vals <- vals(br, 'Bitrate', c(-Inf, 300, 1000, 2000, 5000, 10000, Inf))
fr_vals <- vals(fr, 'Framerate', c(-Inf, 20, 30, 60, Inf))
res_vals <- vals(res, 'Resolution', c(-Inf, 480, 720, 1080, Inf))

## Saving the Graphs to the desktop
# Distributions
save_the_plot(vq_dims, 'vq_dims')
save_the_plot(br_dens, 'bitrate_dens')
save_the_plot(fr_dens, 'framerate_dens')
save_the_plot(res_dens, 'resolution_dens')

save_the_plot(vq_dens, 'vq_dens')
save_the_plot(frag_dens, 'frag_dens')
save_the_plot(disc_dens, 'disc_dens')
save_the_plot(uncl_dens, 'uncl_dens')

# Techniques Comparison
save_the_plot(corr_plot, 'corr_plot')
save_the_plot(rmse_plot, 'rmse_plot')
save_the_plot(rsq_plot, 'rsq_plot')
save_the_plot(bp, 'comp_of_techniques')

# Independent Variables acc to the list order
save_the_plot(br_vals, 'br_vals')
save_the_plot(fr_vals, 'fr_vals')
save_the_plot(res_vals, 'res_vals')

## Saving the models to the Desktop
# sink('~/Desktop/model_equations.txt')
# vid_qual_frag_disc_uncl
# vid_qual_br_fr_res
# frag_br_fr_res
# disc_br_fr_res
# uncl_br_fr_res
# sink()