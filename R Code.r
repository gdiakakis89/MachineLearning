 
setwd("C:/Users/KATSAPRAKAKI/Documents/Project") 
suppressMessages(source("Functions Used for Wine Quality Analyses.R")) 
opts_chunk$set(fig.width=8, fig.height=8)

con <- dbConnect(PostgreSQL(), host="localhost", user= "postgres", password="giannis89", dbname="WineQuality")
con

red_wine_original <- read.csv(file="winequality-red.csv", header=TRUE, sep=";")
str(red_wine_original)

red_wine = red_wine_original
dbWriteTable(con, "red_wine_original", red_wine_original)
dbWriteTable(con, "red_wine", red_wine)

white_wine_original <- read.csv(file="winequality-white.csv", header=TRUE, sep=";")
str(white_wine_original)

white_wine = white_wine_original
dbWriteTable(con, "white_wine_original", white_wine_original)
dbWriteTable(con, "red_wine", red_wine)

# Apply transformations to make the distributions more symetric and less skewed 
red_wine[,c(1:2, 4:11)] <- log(red_wine[,c(1:2, 4:11)]); red_wine[,3] <- red_wine[,3]^0.6
dbRemoveTable(con, "red_wine")
dbWriteTable(con, "red_wine", red_wine)

white_wine[, c(1:2, 4:6, 8:11)] <- log(white_wine[, c(1:2, 4:6, 8:11)]);  white_wine[,3] <- white_wine[,3]^0.45 
dbRemoveTable(con, "white_wine")
dbWriteTable(con, "white_wine", white_wine)
  
# Standardize the variables to make them have mean=0 and variance=1 
red_wine[,1:11] <- as.data.frame( scale( red_wine[,1:11] ))
dbRemoveTable(con, "red_wine")
dbWriteTable(con, "red_wine", red_wine)

white_wine[,1:11] <-as.data.frame( scale( white_wine[,1:11]))
dbRemoveTable(con, "white_wine")
dbWriteTable(con, "white_wine", white_wine)
 
draw_boxplots(red_wine, n_row=3, n_col=4, n_tot=12)    # Red Wine
draw_boxplots(white_wine, n_row=3, n_col=4, n_tot=12)  # White Wine
  
# Show correlation matrix after preprocessings
draw_cor_mat(red_wine)    # Red Wine
draw_cor_mat(white_wine)  # White Wine
  
# Check the normality assumption of Quality for Red Wine  
par(mfrow=c(2,2))

# Check the normality assumption of Quality for Red Wine  
check_normality(red_wine$quality, "Red Wine Quality")   
check_normality(white_wine$quality, "White Wine Quality")

# Fit a linear regression model with stepwise variable selection for Red Wine
summary(fit_fwd_red <- fit_lm_step(red_wine)); 

# Fit a linear regression model with stepwise variable selection for White Wine
summary(fit_fwd_white <- fit_lm_step(white_wine))

vi_boot_red <- boot.relimp(fit_fwd_red, b = 500, type = "lmg", rank = TRUE, diff = TRUE, rela = TRUE)
plot(booteval.relimp(vi_boot_red,sort=TRUE), title="Relative Importance of Predictors for Red Wine")

vi_boot_white <- boot.relimp(fit_fwd_white, b = 500, type = "lmg", rank = TRUE, diff = TRUE, rela = TRUE)
plot(booteval.relimp(vi_boot_white,sort=TRUE), title="Relative Importance of Predictors for White Wine")

par(mfrow=c(1,2))
suppressWarnings(cv.lm(data = red_wine, fit_fwd_red,  m=5))      # Overall MSE = 0.42 
suppressWarnings(cv.lm(data = white_wine, fit_fwd_white,  m=5))  # Overall MSE = 0.56

# Prediction Error using Training-Test Split with Bootstrap resampling
set.seed(123)
get_accuracy_lm(fit_fwd_red, red_wine)     
get_accuracy_lm(fit_fwd_white, white_wine)

## Categorizing Quality into 3 Levels: -1 (bad), 0 (normal), +1 (good) and set 3 colors 
red_wine_sub <- get_subset(red_wine, "Red")$sub
dbWriteTable(con, "red_wine_sub", red_wine_sub)

red_quality_col <- get_subset(red_wine, "Red")$col
dbWriteTable(con, "red_quality_col", red_quality_col)

white_wine_sub <- get_subset(white_wine, "White")$sub
dbWriteTable(con, "white_wine_sub", white_wine_sub)

white_quality_col <- get_subset(white_wine, "White")$col
dbWriteTable(con, "white_quality_col", white_quality_col)

## PCA (principal Component Analysis) 
prin_comp_anal(red_wine_sub, red_quality_col, 0.9)        # Red Wine
prin_comp_anal(white_wine_sub, white_quality_col, 0.8)    # White Wine

## Pairwise scatter plot
pairwise_scatter(red_wine_sub, red_quality_col)
pairwise_scatter(white_wine_sub, white_quality_col)

## Parellel Coordinates Plot
parallel_coordinate_plot(red_wine_sub, 0.5, red_quality_col)
parallel_coordinate_plot(white_wine_sub, 0.2, white_quality_col)

##  Random Forest for predicting truely exceptional (or poor) wines 
set.seed(1)
fit_rf(red_wine, 1000)     # Red Wine
fit_rf(white_wine, 1000)   # White Wine
