###
###  Functions used for the Wine Quality Analyses 
###  By:   David Yang (xdyang70@gmail.com)
###  Date:  June 20, 2016

# Load packages used for the analyses
require(ggplot2)
require(reshape2)
require(randomForest)
suppressWarnings(require(caret))
require(e1071)
require(MASS)
require(relaimpo)
require(DAAG)   # Used fro K-fold cross validation
require(dendextend)
require(gplots)
require(knitr)
require(RPostgreSQL)

# Get upper triangle of a correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Enter to continue 
enter_to_continue <- function() {
  cat ("Press [enter] to continue"); line <- readline() 
} 


# Plot heat map for the triangler correlation matrix in a data set
draw_cor_mat <- function(data_set) {
  ggheatmap  <- ggplot(data = melt(get_upper_tri(round(cor(data_set, use="p"), 2)), na.rm = TRUE), aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson Correlation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+ coord_fixed()
  
  ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))
}

# Draw boxplots for all the columns to check for distribution of variables 
draw_boxplots <- function(data_set, n_row, n_col, n_tot) {
    par(mfrow=c(n_row, n_col))
    for (i in 1:n_tot) {
        boxplot(data_set[,i], xlab=names(data_set)[i])
    }
}

# Check the normality of a variable 
check_normality <- function(y, name) {
  qqnorm(y, main = paste("Normal Q-Q Plot for ", name, sep="")); qqline(y)  # Q-Q Plot
  hist(y, main = paste("Histogram of ", name, sep=""), xlab=name)               # Histogram
}

# Fit a linear regression model with stepwise variable selection
fit_lm_step <- function(wine) {
  min_model = lm(quality ~ 1, data=wine)
  full <- formula( lm(quality ~ . - quality, data=wine))
  fit_fwd_selection = step(min_model, direction='forward', scope=full, trace = 0)
  return (fit_fwd_selection)
}

#Calcuate the prediction accuracy for a linear regression model
get_accuracy_lm <- function (lm_model, wine) {
  set.seed(123)
  tmp_accuracy1 <- 0 
  tmp_accuracy2 <- 0
  for (i in 1:100) {
    samp <- sample(nrow(wine), 0.7 * nrow(wine))
    wine_train <- wine[samp,];  
    wine_test <- wine[-samp,]
    fit_train <- lm(formula=formula(lm_model), data=wine_train)
    test_prediction <- cbind(wine_test$quality, predict(fit_train, wine_test, se.fit=TRUE)$fit)
    tmp_accuracy1 = tmp_accuracy1 + sum(abs(test_prediction[,1] - test_prediction[,2])<0.5) / nrow(test_prediction)
    tmp_accuracy2 = tmp_accuracy2 + sum(abs(test_prediction[,1] - test_prediction[,2])<1) / nrow(test_prediction)
  } 
  return (c (tmp_accuracy1/100,     # We have 59% chance making prediction within 0.5 from true value
             tmp_accuracy2/100))   # We have 89% chance making prediction within 1.0 from true value
}

# choose subset columns of data 
get_subset <- function (wine, type) {
  wine$qlevel <- ifelse(wine$quality < 6, "low", "top")
  wine$qlevel[wine$quality == 6] <- "normal"
  wine$qlevel <- as.factor(wine$qlevel)
  if (type == 'Red') {
    wine_sub <-  wine[,c(11, 2, 10)] 
  }
  else {
    wine_sub <-  wine[,c(11, 2, 10)]
    wine_sub <-  white_wine[, c(11, 8, 2, 6, 5)]
  }
  quality_col <- as.numeric(wine$qlevel) + 1
  return (list(sub=wine_sub, col=quality_col))   
}


# Pairwise scatter plot
pairwise_scatter <- function(wine_sub, quality_col) {
  pairs(wine_sub, col = quality_col, lower.panel = NULL, pch=19, cex=0.2)
  par(xpd = TRUE)
  legend(x = 0.1, y = 0.5, cex = 1, legend = c("low", "normal", "top"), fill = c(2, 3, 4))
  par(xpd = NA)
  
} 

# Parallel Coordinate Plot
parallel_coordinate_plot <- function (wine_sub, frac, quality_col) {
  par(las = 1, mar = c(4, 3, 3, 2) + 0.1, cex = 1)
  set.seed(123456789); 
  samp <- sample(nrow(wine_sub), frac * nrow(wine_sub))
  MASS::parcoord(wine_sub[samp,], col = quality_col[samp], var.label = TRUE, lwd = 0.5)
  legend(x = 1.5, y = 0.1, cex = 1, legend = c("low", "normal", "top"), fill = c(2,3,4), horiz = TRUE)
}


# Draw Heatmap together with Dendragram 
draw_dend <- function(data, sub_prop, obj_class, obj_labels, colors) {
  
    d_wine <- dist(data)
    hc_wine <- hclust(d_wine, method = "complete")

    dend <- as.dendrogram(hc_wine)
    dend <- rotate(dend, 1:(nrow(data)))

    labels_colors(dend) <- colors[sort_levels_values(as.numeric(obj_labels)[order.dendrogram(dend)])]
    labels(dend) <- paste(as.character(obj_labels)[order.dendrogram(dend)], "(",labels(dend),")", sep = "")
    dend <- hang.dendrogram(dend,hang_height=0.1)
    dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
    dend <- set(dend, "labels_cex", 0.5)
    
    par(mar = c(3,3,3,7))
    plot(dend, horiz =  TRUE,  nodePar = list(cex = .007))
    legend("topleft", legend = (obj_class), fill = colors)
    
    return(dend)
}

# Dendragram and hierarchical cluster tree out together
draw_dend_hclust <-  function(data_matrix, dend) { 
    gplots::heatmap.2(as.matrix(data_matrix), 
                      main = "Heatmap for the Wine data set",
                      dendrogram = "row",
                      Rowv = dend,
                      Colv = "NA", # this to make sure the columns are not ordered
                      trace="none",          
                      margins =c(5, 0.1),      
                      denscol = "grey",
                      density.info = "density",
                      RowSideColors = (labels_colors(dend)) # to add nice colored strips        
    )
}

# PCA Analysis with plots
prin_comp_anal <- function(wine_sub, quality1, sz) {
    wine_pca <- prcomp(wine_sub, center = TRUE, scale. = TRUE) 
    #plot(wine_pca, type = "l")
    summary(wine_pca)
    wine_pcs <- as.data.frame(cbind(predict(wine_pca, newdata=wine_sub), quality=quality1))
    ggplot(wine_pcs, aes(PC1, PC2)) + 
      geom_point(aes(colour = factor(quality)), alpha = 1, size = sz) +
      scale_colour_discrete(name  ="Quality", labels=c("Low", "Normal", "Top"))
}


# Fit Random Forest to the Data
fit_rf <- function (wine, n) {

  wine$qlevel <- ifelse(wine$quality < 6, 'low', 'top')
  wine$qlevel[wine$quality == 6] <- 'normal'
  wine$qlevel <- as.factor(wine$qlevel)
  
  samp <- sample(nrow(wine), 0.7 * nrow(wine))
  fit_rf <- randomForest(qlevel ~ . - quality, data = wine[samp, ], ntree=n)
  pred <- predict(fit_rf, newdata = wine[-samp, ])
  conf_tbl <- table(pred, wine[-samp, ]$qlevel)
  print(conf_tbl)
  
  print((conf_tbl[1,1] + conf_tbl[2,2] + conf_tbl[3,3]) / nrow(wine[-samp, ]))
  
  varImpPlot(fit_rf, type=2) 
}

# Fit Random Forest to the Data
fit_rf_simple <- function (wine, n, type) {
  
  wine$qlevel <- ifelse(wine$quality < 6, 'low', 'top')
  wine$qlevel[wine$quality == 6] <- 'normal'
  wine$qlevel <- as.factor(wine$qlevel)
  
  samp <- sample(nrow(wine), 0.7 * nrow(wine))
  if (type == "Red")
    fit_rf <- randomForest(qlevel ~ alcohol + sulphates + volatile.acidity + density + total.sulfur.dioxide, data = wine[samp, ], ntree=n)
  else 
    fit_rf <- randomForest(qlevel ~ alcohol + density + volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + residual.sugar + chlorides, data = wine[samp, ], ntree=n)
  pred <- predict(fit_rf, newdata = wine[-samp, ])
  conf_tbl <- table(pred, wine[-samp, ]$qlevel)
  print(conf_tbl)

  print(conf_tbl[1,1] + conf_tbl[2,2] + conf_tbl[3,3]) / nrow(wine[-samp, ])
}