require(caTools)
require(ggplot2)
require(gridExtra)

dataset <- read.csv('Dataset-G1.csv', sep = ';', stringsAsFactors = F, dec = ',')

## Functions
rsq <- function(x, y) cor(x, y)^2
rmse <- function(x, y) sqrt(mean((x-y)^2))
create_dens <- function(variab, ds) {
  vr <- ds[[variab]]
  bw <- (max(vr)-min(vr))/length(vr)^(1/3)
  # print(bw)
  print(sqrt(length(vr)))
  dens <- ggplot(ds, aes_string(x=variab))+  
    geom_histogram(aes(y=..density..), position = 'identity', alpha=0.6) + 
    # geom_histogram(aes(y=..density..), position = 'identity', alpha=0.6, binwidth = bw) + 
    geom_density(alpha=.4, fill='red') +
    geom_vline(aes(xintercept=mean(ds[,variab])), color='blue', lty=2)+
    scale_color_brewer(palette = 'Dark2') +
    theme_minimal()
  print(dens)
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
sink('~/Desktop/vid_qual_with_indep_vars.txt')
print(summary(vid_qual_br_fr_res))
sink()

## Video Quality dimensions models with independent variables
# Fragmentation with independent variables
frag_br_fr_res <- lm(frag ~ br + fr + res)
sink('~/Desktop/frag_with_indep_vars.txt')
print(summary(frag_br_fr_res))
sink()

# Discontinuity with independent variables
disc_br_fr_res <- lm(disc ~ br + fr + res)
sink('~/Desktop/disc_with_indep_vars.txt')
print(summary(disc_br_fr_res))
sink()

# Unclearness with independent variables
uncl_br_fr_res <- lm(uncl ~ br + fr + res)
sink('~/Desktop/uncl_with_indep_vars.txt')
print(summary(uncl_br_fr_res))
sink()

# Video Quality model with quality dimension models
vid_qual_frag_disc_uncl <- lm(vq ~ frag + disc + uncl)
sink('~/Desktop/vid_qual_with_dep_vars.txt')
print(summary(uncl_br_fr_res))
sink()


# Video Quality model with quality dimension models predicted from independent vars
br_fr_res <- data.frame(br=br, fr=fr, res=res) # Create the data frame to use in the predict function
predicted_frag <- predict.lm(frag_br_fr_res, br_fr_res)
predicted_disc <- predict.lm(disc_br_fr_res, br_fr_res)
predicted_uncl <- predict.lm(uncl_br_fr_res, br_fr_res)

vid_qual_with_predicted_frag_disc_uncl <- lm(vq ~ predicted_frag + predicted_disc + predicted_uncl)
sink('~/Desktop/vid_qual_with_predicted_frag_disc_uncl.txt')
print(summary(vid_qual_with_predicted_frag_disc_uncl))
sink()


## Get the result of the comparison between test and predicted data
br_fr_res_test <- data.frame(br=br_test, fr=fr_test, res=res_test)
frag_disc_uncl_test <- data.frame(frag=frag_test, disc=disc_test, uncl=uncl_test)

# Video Quality Model that is created with independent vars
pred_vid_qual_with_ind_vars <- predict(vid_qual_br_fr_res, br_fr_res_test)
comp_ind <- data.frame(cbind(actuals=vq_test, predicteds=pred_vid_qual_with_ind_vars))
cor_ind <- cor(comp_ind$actuals, comp_ind$predicteds)
rsq_ind <- rsq(comp_ind$actuals, comp_ind$predicteds)
rmse_ind <- rmse(comp_ind$actuals, comp_ind$predicteds)

# BURDA TNAIMLANAN MODELE IHTIYAC YOK GALIBA, CUNKU VIDEO KALITESI BAGIMSIZ DEGISKENLERDEN ELDE EDILIYO OLMASI GEREK
# Video Quality Model that is created with dependent vars
# pred_vid_qual_with_dep_vars <- predict(vid_qual_frag_disc_uncl, frag_disc_uncl_test)
# actuals_preds_vid_qual_with_dep_vars <- data.frame(cbind(actuals=vq_test, predicteds=pred_vid_qual_with_dep_vars))
# cor_vid_qual_with_dep_vars <- cor(actuals_preds_vid_qual_with_dep_vars$actuals, actuals_preds_vid_qual_with_dep_vars$predicteds)
# rsq_vid_qual_with_dep_vars <- rsq(actuals_preds_vid_qual_with_dep_vars$actuals, actuals_preds_vid_qual_with_dep_vars$predicteds)
# rmse_vid_qual_with_dep_vars <- rmse(actuals_preds_vid_qual_with_dep_vars$actuals, actuals_preds_vid_qual_with_dep_vars$predicteds)

# Video Quality Model that is created with hybrid technique
pred_vid_qual_with_both <- predict(vid_qual_with_predicted_frag_disc_uncl,  data.frame(predicted_frag=frag_disc_uncl_test$frag, predicted_disc=frag_disc_uncl_test$disc, predicted_uncl=frag_disc_uncl_test$uncl))
comp_both <- data.frame(cbind(actuals=vq_test, predicteds=pred_vid_qual_with_both))
cor_both <- cor(comp_both$actuals, comp_both$predicteds)
rsq_both <- rsq(comp_both$actuals, comp_both$predicteds)
rmse_both <-rmse(comp_both$actuals, comp_both$predicteds)


# Visualising the data
col.1 <- c("Direct", "Direct", "Direct", "Indirect", "Indirect", "Indirect")
col.2 <- c(cor_ind, rsq_ind, rmse_ind, cor_both, rsq_both, rmse_both)
col.3 <- c("Correlation", "RSQ", "RMSE", "Correlation", "RSQ", "RMSE")

df1 <-  matrix(c(col.1,round(col.2, 2), col.3), nrow = 6, ncol = 3) 
colnames(df1) <- c("Dir_Indir", "Val", "Type")
df1 <- data.frame(df1)

bp1 <- ggplot(data=df1,
              aes(x=df1$Type,
                  y=df1$Val,
                  # y=as.factor(seq(0, 1, length.out = nrow(df1))),
                  fill=df1$Dir_Indir,
                  group=df1$Dir_Indir))+
  geom_bar(stat='identity', position = position_dodge(), color='black')+
  geom_text(aes(label=df1$Val), color='black', position = position_dodge2(1), size=4.5, vjust=-0.5)+
  theme_minimal() +
  labs(title = "Comparison of Two Techniques", x="Different Metrics", y="Values", fill="Techniques")+
  scale_fill_manual(values = c('#77E3C4', '#FFAF7B'))
bp1

# Create density distributions of the independent vars
vq_dens <- create_dens('VQ', dataset)

frag_dens <- create_dens('VF', dataset)
disc_dens <- create_dens('VD', dataset)
uncl_dens <- create_dens('VU', dataset)

br_dens <- create_dens('Bitrate', dataset)
fr_dens <- create_dens('Framerate', dataset)
res_dens <- create_dens('Resolution', dataset)
br_dens
vq_dims <- grid.arrange(frag_dens, disc_dens, uncl_dens)
ind_vars <- grid.arrange(br_dens, fr_dens, res_dens)


## Showing independent variables for the video quality
# Showing the bitrate values for the list order
ind_vars <- ggplot(dataset)+
  theme(legend.position = 'none')+
  labs(title='Independent Variable', x = "Order In The List")

br_vals <- ind_vars +  
  geom_point(aes(x=seq(1, length(Bitrate)), 
                 y=Bitrate, 
                 colour=cut(Bitrate, c(-Inf, 300, 1000, 2000, 5000, 10000, Inf))))
br_vals

# Showing the framerate values for the list order
fr_vals <- ind_vars+
  geom_point(aes(x=seq(1, length(Framerate)),
                 y=Framerate,
                 ))
fr_vals

# Showing the resolution values for the list order
res_vals <- ind_vars+
  geom_point(aes(x=seq(1, length(Resolution)),
                 y=Resolution,
                 colour=cut(Resolution, c(-Inf, 480, 720, 1080, Inf))))
res_vals

## Saving the Graphs to the desktop
# Distributions
# ggsave('~/Desktop/ind_vars.png', plot = ind_vars)
ggsave('~/Desktop/vq_dims.png', plot = vq_dims)
ggsave('~/Desktop/br.png', plot = br_dens)
ggsave('~/Desktop/fr.png', plot = fr_dens)
ggsave('~/Desktop/res.png', plot = res_dens)

ggsave('~/Desktop/vq_dens.png', plot = vq_dens)
ggsave('~/Desktop/frag_dens.png', plot = frag_dens)
ggsave('~/Desktop/disc_dens.png', plot = disc_dens)
ggsave('~/Desktop/uncl_dens.png', plot = uncl_dens)

# Technique Comparison
# All three included
ggsave('~/Desktop/comp_of_techniques.png', plot = bp1, device = 'png')

# Independent Variables acc to the list order
ggsave('~/Desktop/br_vals.png', plot = br_vals)
ggsave('~/Desktop/fr_vals.png', plot = fr_vals)
ggsave('~/Desktop/res_vals.png', plot = res_vals)


## Saving the models to the Desktop
# sink('~/Desktop/model_equations.txt')
# vid_qual_frag_disc_uncl
# vid_qual_br_fr_res
# frag_br_fr_res
# disc_br_fr_res
# uncl_br_fr_res
# sink()





































































































