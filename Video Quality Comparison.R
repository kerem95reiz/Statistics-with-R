require(caTools)
require(ggplot2)

dataset <- read.csv('Dataset-G1.csv', sep = ';', stringsAsFactors = F, dec = ',')

## Functions
rsq <- function(x, y) cor(x, y)^2
rmse <- function(x, y) sqrt(mean((x-y)^2))

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
direct <- c(cor_ind, rmse_ind, rsq_ind)
indirect <- c(cor_both, rmse_both, rsq_both)

col.1 <- c(cor_ind, cor_both)
col.2 <- c(rmse_ind, rmse_both)
col.3 <- c(rsq_ind, rsq_both)

matr <- matrix(c(col.1, col.2, col.3), nrow = 2, ncol = 3)
colnames(matr) <- c("Correlation", "RMSE", "RSQ")
rownames(matr) <- c("Direct", "Indirect")

bp <- barplot(matr, beside = T, legend.text = rownames(matr), ylim = c(0, max(matr)+0.5), col = c('#77E3C4', '#FFAF7B'))

# bp
# legend(x=max(matr)-0.2, y=max(matr)-0.2, legend = rownames(matr))
# text(x=bp, label = matr)

cols <- c("Direct", "Indirect", "Var")
row.1 <- c(cor_ind, cor_both, "Correlation")
row.2 <- c(rmse_ind, rmse_both, "RMSE")
row.3 <- c(rsq_ind, rsq_both, "RSQ")

df <- t(matrix(c(row.1, row.2, row.3), nrow = 3, ncol = 3))
colnames(df) <- cols
df <- data.frame(df)
bp <- ggplot(data = df,
             aes(x=df$Var,
                 y=df$Direct,
                 fill=colnames(df)))+
  geom_bar(stat = 'identity')
bp

## Ikinci deneme
col.1 <- c("Direct", "Direct", "Direct", "Indirect", "Indirect", "Indirect")
col.2 <- c(cor_ind, rsq_ind, rmse_ind, cor_both, rsq_both, rmse_both)
col.3 <- c("Correlation", "RSQ", "RMSE", "Correlation", "RSQ", "RMSE")

df1 <-  matrix(c(col.1,round(col.2, 2), col.3), nrow = 6, ncol = 3) 
colnames(df1) <- c("Dir_Indir", "Val", "Type")
df1 <- data.frame(df1)
bp1 <- ggplot(data=df1,
              aes(x=df1$Type,
                  y=df1$Val,
                  # y=as.factor(seq(0.0, 1.0, length.out = nrow(df1))),
                  fill=df1$Dir_Indir))+
  geom_bar(stat='identity', position = position_dodge(), color='black')+
  geom_text(aes(label=df1$Val), color='black', position = position_dodge(0.9), size=4.5)+
  theme_minimal() +
  # coord_fixed(ratio = 0.2) +
  scale_fill_manual(values = c('#77E3C4', '#FFAF7B'))
bp1

ggsave('~/Desktop/cor_rmse_rsq.png')











