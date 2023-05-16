install.packages("RStoolbox")
library(glcm)
library(raster)
library(rasterVis)    # raster visualisation    
library(sp)               # Spatial data
library(rgdal) 
library(mgcv)
library(glmnet)
library(GLCMTextures)




###
sapply(1:159, function(i) unique(c(roi.list[[i]][,22,])))
###Test
grey_scale_img <- channel(pt1_test[,2,], "gray")
glcm_matrix <- glcm(grey_scale_img, window = c(3, 3), shift = c(1, 1))
features <- summary(glcm_matrix)
features
###
#Attempt 1 GLCM
test_matrix<- matrix(data=c(2,0,1,3,0,0,0,3,2), nrow = 3, ncol=3)
print(test_matrix)
horizontal_glcm<- make_glcm(test_matrix, n_levels = 4, shift = c(1,0), normalize = FALSE)
horizontal_glcm


glcm_mean_mean <- sum(as.matrix(rglcm_pt1$glcm_mean),
                          na.rm = TRUE)/(length(as.matrix(rglcm_pt1$glcm_mean))
                                         - sum(is.na(as.matrix(rglcm_pt1$glcm_mean))))


stack(pt1_test[,2,], rglcm_pt1$glcm_mean)

#Quantize
ras_obj <- rast(pt1_test[,2,])
rq_equalrange<- quantize_raster(r = ras_obj, n_levels = 32, method = "equal range")
plot(rq_equalrange, col=grey.colors(32))
plot(rq_equalrange, col=grey.colors(16))

test_glcm_text <- glcm_textures(rq_equalrange, w = c(3,5), n_levels = 32, quantization = "none", shift = c(1,0))
test_glcm_text

#Attempt
symm_glcm <- make_glcm(rq_equalrange, n_levels=32, shift=c(1, 0), normalize=TRUE)
symm_glcm_norm <- symm_glcm/sum(symm_glcm)
unique(c(symm_glcm))
glcm_metrics(symm_glcm)


str(roi.list)
pt1_test <- roi.list[[1]]
plot(raster(pt1_test[,2,]))
nlevels(as.factor(pt1_test[,2,]))

unique(c(pt1_test[,2,]))
symm_glcm <- make_glcm(pt1_test[,2,], n_levels=58, shift=c(1, 0), normalize=TRUE)
symm_glcm_norm <- symm_glcm/sum(symm_glcm)
unique(c(symm_glcm))
glcm_metrics(symm_glcm)

for (i in 1:n){
  for (j in 1:m){
    contrast[i, j] <- unname(glcm_metrics(roi.list[[i]][,j,])[1])
    
  }
}








img_texture <- function(img, n_row, n_col){
  for (i in 1:n_row){
    for (j in 1:n_col){
      rglcm_pt <- glcm(raster(img[[i]][,j,]), 
                                   window = c(9,9), 
                                   shift = c(1,1), 
                                   statistics = c("mean", "variance", "homogeneity", "contrast", 
                                                  "dissimilarity", "entropy", "second_moment")
                  )
      glcm_mean_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_mean),
                            na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_mean))
                                           - sum(is.na(as.matrix(rglcm_pt$glcm_mean))))
      
  
  }
  }
  return(glcm_mean_mean)
}







#Attempt Function 2
final_subj <- plot.dat.1.29_hippo[plot.dat.1.29_hippo$DIAGNOSIS != 2, "PTID"]
subj.names

subj_of_interest <- which(subj.names %in% final_subj)

n_row <- 159
n_col <- 21
glcm_mean_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_var_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_homog_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_contrast_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_dissim_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_entropy_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_second_moment_mean <- matrix(NA, nrow=n_row, ncol=n_col)

img_texture_mean <- function(img, subj_of_interest, n_col){
  for (i in subj_of_interest){
    for (j in 1:n_col){
      if (length(unique(raster(img[[i]][,j,]))) == 1) {
        glcm_mean_mean[i, j] <- NA
        next
      }
      
      rglcm_pt <- glcm(raster(img[[i]][,j,]), 
                       window = c(9,9), 
                       shift = c(1,1), 
                       statistics = c("mean", "variance", "homogeneity", "contrast", 
                                      "dissimilarity", "entropy", "second_moment")
      )
      glcm_mean_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_mean),
                                  na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_mean))
                                                 - sum(is.na(as.matrix(rglcm_pt$glcm_mean))))
      
      
    }
  }
  return(glcm_mean_mean)
}

glcm_mean_mat <- img_texture_mean(roi.list, subj_of_interest, n_col)
# Identify rows where all elements are NA
all_na_glcm_mean <- apply(glcm_mean_mat, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_mean <- glcm_mean_mat[!all_na_glcm_mean, ]







glcm_second_moment_mean <- glcm_second_moment_mean[complete.cases(glcm_second_moment_mean),]
glcm_second_moment_mean




















final_subj <- plot.dat.1.29_hippo[plot.dat.1.29_hippo$DIAGNOSIS != 2, "PTID"]
subj.names
subj_of_interest <- which(subj.names %in% final_subj)

n_row <- 159
n_col <- 21
glcm_mean_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_var_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_homog_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_contrast_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_dissim_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_entropy_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_second_moment_mean <- matrix(NA, nrow=n_row, ncol=n_col)
glcm_correlation_mean <- matrix(NA, nrow=n_row, ncol=n_col)

#Attempt 3
for (i in subj_of_interest){
  for (j in 1:n_col){
    if (length(unique(raster(roi.list[[i]][,j,]))) == 1) {
      glcm_mean_mean[i, j] <- NA
      glcm_var_mean[i, j] <- NA
      glcm_homog_mean[i, j] <- NA
      glcm_contrast_mean[i, j] <- NA
      glcm_dissim_mean[i, j] <- NA
      glcm_entropy_mean[i, j] <- NA
      glcm_second_moment_mean[i, j] <- NA
      glcm_correlation_mean[i, j] <- NA
      next
    }
    
    rglcm_pt <- glcm(raster(roi.list[[i]][,j,]), 
                     window = c(9,9), 
                     shift = c(1,1), 
                     statistics = c("mean", "variance", "homogeneity", "contrast", 
                                    "dissimilarity", "entropy", "second_moment", "correlation")
    )
    glcm_mean_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_mean),
                                na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_mean))
                                               - sum(is.na(as.matrix(rglcm_pt$glcm_mean))))
    glcm_var_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_variance),
                                na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_variance))
                                               - sum(is.na(as.matrix(rglcm_pt$glcm_variance))))
    glcm_homog_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_homogeneity),
                                na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_homogeneity))
                                               - sum(is.na(as.matrix(rglcm_pt$glcm_homogeneity))))
    glcm_contrast_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_contrast),
                                 na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_contrast))
                                                - sum(is.na(as.matrix(rglcm_pt$glcm_contrast))))
    glcm_dissim_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_dissimilarity),
                                 na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_dissimilarity))
                                                - sum(is.na(as.matrix(rglcm_pt$glcm_dissimilarity))))
    glcm_entropy_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_entropy),
                                 na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_entropy))
                                                - sum(is.na(as.matrix(rglcm_pt$glcm_entropy))))
    glcm_second_moment_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_second_moment),
                                 na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_second_moment))
                                                - sum(is.na(as.matrix(rglcm_pt$glcm_second_moment))))
    glcm_correlation_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_correlation),
                                         na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_correlation))
                                                        - sum(is.na(as.matrix(rglcm_pt$glcm_correlation))))
    
  }
}
### Mean
# Identify rows where all elements are NA
all_na_glcm_mean <- apply(glcm_mean_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_mean <- glcm_mean_mean[!all_na_glcm_mean, ]
mat_glcm_mean

### Var
# Identify rows where all elements are NA
all_na_glcm_var <- apply(glcm_var_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_var <- glcm_var_mean[!all_na_glcm_var, ]
mat_glcm_var

### Homog
# Identify rows where all elements are NA
all_na_glcm_homog <- apply(glcm_homog_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_homog <- glcm_homog_mean[!all_na_glcm_homog, ]
mat_glcm_homog

### Contrast
# Identify rows where all elements are NA
all_na_glcm_contrast <- apply(glcm_contrast_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_contrast <- glcm_contrast_mean[!all_na_glcm_contrast, ]
mat_glcm_contrast

### Dissim
# Identify rows where all elements are NA
all_na_glcm_dissim <- apply(glcm_dissim_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_dissim <- glcm_dissim_mean[!all_na_glcm_dissim, ]
mat_glcm_dissim

### Entropy
# Identify rows where all elements are NA
all_na_glcm_entropy <- apply(glcm_entropy_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_entropy <- glcm_entropy_mean[!all_na_glcm_entropy, ]
mat_glcm_entropy

### Second Moment
# Identify rows where all elements are NA
all_na_glcm_secondmom <- apply(glcm_second_moment_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_secondmom <- glcm_second_moment_mean[!all_na_glcm_secondmom, ]
mat_glcm_secondmom

### Correlation
# Identify rows where all elements are NA
all_na_glcm_correlation <- apply(glcm_correlation_mean, 1, function(row) all(is.na(row)))
# Remove rows with all NA values
mat_glcm_correlation <- glcm_correlation_mean[!all_na_glcm_correlation, ]
mat_glcm_correlation



#Clinical Merge
library(caret)
clin_table <- clinical[which(clinical$PTID %in% final_subj),]
table(clin_table$DIAGNOSIS)

clin_table_df <- clinical_hip_reg_fin[which(clinical_hip_reg_fin$PTID %in% final_subj),]

slice_num <- seq(1, 21)
slices <- paste0("Slice_", slice_num)

#Cross-validated GLCM#
#####GLCM Mean#####
set.seed(60)
colnames(mat_glcm_mean) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_mean)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Variance#####
set.seed(50)
colnames(mat_glcm_var) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_var)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Homog#####
set.seed(50)
colnames(mat_glcm_homog) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_homog)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Contrast#####
set.seed(50)
colnames(mat_glcm_contrast) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_contrast)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Dissim#####
set.seed(50)
colnames(mat_glcm_dissim) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_dissim)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Entropy#####
set.seed(50)
colnames(mat_glcm_entropy) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_entropy)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")

#####GLCM Second Moment#####
set.seed(50)
colnames(mat_glcm_secondmom) <- slices
data_mean_reg <- cbind(clin_table_df, mat_glcm_secondmom)
data_mean_reg$diagnosis <- ifelse(data_mean_reg$DIAGNOSIS == 1, "CN", "AD")



set.seed(50) # For reproducibility #123
data <- data_mean_reg[,-c(1, 2, 4, 7)]
data_without_na <- data[complete.cases(data), ]
unstd_mat <- model.matrix(diagnosis ~ ., data=data_without_na)[,-1] 
X_mat <- apply(unstd_mat[,-2], 2, function(x) (x - mean(x)) / (2 * sd(x)))
#X_mat <- rescale(as.vector(unstd_mat[,-2]), binary.inputs="full")
X_standardized <- data.frame(AGE=X_mat[,1], PTGENDERMale=unstd_mat[,2], X_mat[,-1])
Y <- as.factor(data_without_na$diagnosis)



set.seed(50)
cv_control <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE
)

##### Slices Only #####
data <- data_mean_reg[,-c(1:7)]
data_without_na <- data[complete.cases(data), ]
unstd_mat <- model.matrix(diagnosis ~ ., data=data_without_na)[,-1] 
X_standardized <- apply(unstd_mat, 2, function(x) (x - mean(x)) / (2 * sd(x)))
Y <- as.factor(data_without_na$diagnosis)

set.seed(50)
# Train the model using repeated 5-fold cross-validation
model <- train(x = X_standardized,
               y = Y,
               method = "glmnet",
               family = "binomial",
               trControl = cv_control,
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length.out = 100)))
summary(model)
model$resample
model$bestTune
best_lambda <-model$bestTune$lambda
best_coefficients <- coef(model$finalModel, s = best_lambda)
print(best_coefficients)

# Get the names of the selected variables
selected_variables <- rownames(best_coefficients)[which(best_coefficients != 0)][-1]
selected_variables 

mean(model$resample$ROC)
sd(model$resample$ROC)
mean(model$resample$Sens)
sd(model$resample$Sens)
mean(model$resample$Spec)
sd(model$resample$Spec)
best_lambda

model$results[which(model$results$lambda == best_lambda),]



#Reduced Model
###Reduced Model###
#Take the mean across slices
data_mean_reg$Slice_Mean <- rowMeans(data_mean_reg[,6:26])




















#####GLCM Mean#####
set.seed(60)
colnames(mat_glcm_mean) <- slices
data_mean_reg <- cbind(clin_table, mat_glcm_mean)
data_mean_reg$DIAGNOSIS <- as.factor(data_mean_reg$DIAGNOSIS)
data_mean_reg$DIAGNOSIS <- as.factor(ifelse(data_mean_reg$DIAGNOSIS == 1, 0, 1))

reg_pred <- as.formula(paste("DIAGNOSIS ~", paste(slices[1:10], collapse = " + ")))

mod1_glcm <- glm(reg_pred, data=data_mean_reg, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")

confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.5162125)), reference=data_mean_reg$DIAGNOSIS)

write.csv(data_mean_reg, file = "glcm_mean.csv", row.names = FALSE)

#LASSO Regression#
set.seed(60)
slice_matrix <- as.matrix(data_mean_reg[,slices])
diagnosis_vector <- as.vector(data_mean_reg$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
selected_slices <- predict(fit, type="coefficients", s=fit$lambda.min)
best_perform <- coef(fit, s=lambda_min)


vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))

#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.2887359)), reference=data_mean_reg$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4714912)), reference=data_mean_reg$DIAGNOSIS)


###Reduced Model###
#Take the mean across slices
data_mean_reg$Slice_Mean <- rowMeans(data_mean_reg[,6:26])
mod2_glcm <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg, family="binomial")
summary(mod2_glcm)
test_predy_mod2_glcm <- predict(mod2_glcm, newdata=data_mean_reg, "response")
#Find best threshold using ROC Curve
roc_test_glcm2 <- roc(data_mean_reg$DIAGNOSIS, test_predy_mod2_glcm)
coords(roc_test_glcm2, "best")

confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm>0.3581332)), reference=data_mean_reg$DIAGNOSIS)


##### Variance #####
set.seed(60)
colnames(mat_glcm_var) <- slices
data_mean_reg_var <- cbind(clin_table, mat_glcm_var)
data_mean_reg_var$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_var$DIAGNOSIS == 1, 0, 1))
#LASSO
slice_matrix <- as.matrix(data_mean_reg_var[,slices])
diagnosis_vector <- as.vector(data_mean_reg_var$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_var, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_var, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_var$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4850544)), reference=data_mean_reg_var$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.470866)), reference=data_mean_reg_var$DIAGNOSIS)

#Take the contrast across slices
data_mean_reg_var$Slice_Mean <- rowMeans(data_mean_reg_var[,6:26])
mod2_glcm_var <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_var, family="binomial")
summary(mod2_glcm_var)
test_predy_mod2_glcm_var <- predict(mod2_glcm_var, newdata=data_mean_reg_var, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_var <- roc(data_mean_reg_var$DIAGNOSIS, test_predy_mod2_glcm_var)
coords(roc_test_glcm2_var, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_var>0.3542869)), reference=data_mean_reg_var$DIAGNOSIS)




##### Contrast #####
set.seed(60)
colnames(mat_glcm_contrast) <- slices
data_mean_reg_con <- cbind(clin_table, mat_glcm_contrast)
data_mean_reg_con$DIAGNOSIS <- as.factor(data_mean_reg_con$DIAGNOSIS)
data_mean_reg_con$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_con$DIAGNOSIS == 1, 0, 1))

reg_pred_con <- as.formula(paste("DIAGNOSIS ~", paste(slices[1:10], collapse = " + ")))

mod1_glcm_con <- glm(reg_pred_con, data=data_mean_reg_con, family="binomial")
summary(mod1_glcm_con)
test_predy_mod1_glcm_con <- predict(mod1_glcm_con, newdata=data_mean_reg_con, "response")
#Find best threshold using ROC Curve
roc_test_glcm_con <- roc(data_mean_reg_con$DIAGNOSIS, test_predy_mod1_glcm_con)
coords(roc_test_glcm_con, "best")

confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm_con>0.3784804)), reference=data_mean_reg_con$DIAGNOSIS)


#LASSO
set.seed(60)
slice_matrix <- as.matrix(data_mean_reg_con[,slices])
diagnosis_vector <- as.vector(data_mean_reg_con$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
selected_slices <- predict(fit, type="coefficients", s=fit$lambda.min)
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_con, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_con, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_con$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.3367317)), reference=data_mean_reg_con$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.6815285)), reference=data_mean_reg_con$DIAGNOSIS)


#Take the contrast across slices
data_mean_reg_con$Slice_Mean <- rowMeans(data_mean_reg_con[,6:26])
mod2_glcm_con <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_con, family="binomial")
summary(mod2_glcm_con)
test_predy_mod2_glcm_con <- predict(mod2_glcm_con, newdata=data_mean_reg_con, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_con <- roc(data_mean_reg_con$DIAGNOSIS, test_predy_mod2_glcm_con)
coords(roc_test_glcm2_con, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_con>0.3291847)), reference=data_mean_reg_con$DIAGNOSIS)


##### Homogeneity #####
set.seed(60)
colnames(mat_glcm_homog) <- slices
data_mean_reg_homog <- cbind(clin_table, mat_glcm_homog)
data_mean_reg_homog$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_homog$DIAGNOSIS == 1, 0, 1))
#LASSO
slice_matrix <- as.matrix(data_mean_reg_homog[,slices])
diagnosis_vector <- as.vector(data_mean_reg_homog$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_homog, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_homog, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_homog$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.5394674)), reference=data_mean_reg_homog$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.3868396)), reference=data_mean_reg_homog$DIAGNOSIS)

#Take the contrast across slices
data_mean_reg_homog$Slice_Mean <- rowMeans(data_mean_reg_homog[,6:26])
mod2_glcm_homog <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_homog, family="binomial")
summary(mod2_glcm_homog)
test_predy_mod2_glcm_homog <- predict(mod2_glcm_homog, newdata=data_mean_reg_homog, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_homog <- roc(data_mean_reg_homog$DIAGNOSIS, test_predy_mod2_glcm_homog)
coords(roc_test_glcm2_homog, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_homog>0.4553062)), reference=data_mean_reg_homog$DIAGNOSIS)





##### Dissimilarity #####
set.seed(66)
colnames(mat_glcm_dissim) <- slices
data_mean_reg_dissim <- cbind(clin_table, mat_glcm_dissim)
data_mean_reg_dissim$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_dissim$DIAGNOSIS == 1, 0, 1))
#LASSO
slice_matrix <- as.matrix(data_mean_reg_dissim[,slices])
diagnosis_vector <- as.vector(data_mean_reg_dissim$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_dissim, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_dissim, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_dissim$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.5603612)), reference=data_mean_reg_dissim$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.3990449)), reference=data_mean_reg_dissim$DIAGNOSIS)

#Take the contrast across slices
data_mean_reg_dissim$Slice_Mean <- rowMeans(data_mean_reg_dissim[,6:26])
mod2_glcm_dissim <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_dissim, family="binomial")
summary(mod2_glcm_dissim)
test_predy_mod2_glcm_dissim <- predict(mod2_glcm_dissim, newdata=data_mean_reg_dissim, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_dissim <- roc(data_mean_reg_dissim$DIAGNOSIS, test_predy_mod2_glcm_dissim)
coords(roc_test_glcm2_dissim, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_dissim>0.3653425)), reference=data_mean_reg_dissim$DIAGNOSIS)



##### Entropy #####
set.seed(60)
colnames(mat_glcm_entropy) <- slices
data_mean_reg_entropy <- cbind(clin_table, mat_glcm_entropy)
data_mean_reg_entropy$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_entropy$DIAGNOSIS == 1, 0, 1))
#LASSO
slice_matrix <- as.matrix(data_mean_reg_entropy[,slices])
diagnosis_vector <- as.vector(data_mean_reg_entropy$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_entropy, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_entropy, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_entropy$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.2843838)), reference=data_mean_reg_entropy$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4248911)), reference=data_mean_reg_entropy$DIAGNOSIS)

#Take the contrast across slices
data_mean_reg_entropy$Slice_Mean <- rowMeans(data_mean_reg_entropy[,6:26])
mod2_glcm_entropy <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_entropy, family="binomial")
summary(mod2_glcm_entropy)
test_predy_mod2_glcm_entropy <- predict(mod2_glcm_entropy, newdata=data_mean_reg_entropy, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_entropy <- roc(data_mean_reg_entropy$DIAGNOSIS, test_predy_mod2_glcm_entropy)
coords(roc_test_glcm2_entropy, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_entropy>0.3850539)), reference=data_mean_reg_entropy$DIAGNOSIS)

##### Second Moment #####
set.seed(62)
colnames(mat_glcm_secondmom) <- slices
data_mean_reg_secondmom <- cbind(clin_table, mat_glcm_secondmom)
data_mean_reg_secondmom$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_secondmom$DIAGNOSIS == 1, 0, 1))
#LASSO
slice_matrix <- as.matrix(data_mean_reg_secondmom[,slices])
diagnosis_vector <- as.vector(data_mean_reg_secondmom$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
best_perform <- coef(fit, s=lambda_1se)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("DIAGNOSIS ~", paste(vars_include, collapse = " + ")))
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=data_mean_reg_secondmom, family="binomial")
summary(mod1_glcm)
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=data_mean_reg_secondmom, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(data_mean_reg_secondmom$DIAGNOSIS, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.3785387)), reference=data_mean_reg_secondmom$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4101265)), reference=data_mean_reg_secondmom$DIAGNOSIS)

#Take the contrast across slices
data_mean_reg_secondmom$Slice_Mean <- rowMeans(data_mean_reg_secondmom[,6:26])
mod2_glcm_secondmom <- glm(DIAGNOSIS ~ Slice_Mean, data=data_mean_reg_secondmom, family="binomial")
summary(mod2_glcm_secondmom)
test_predy_mod2_glcm_secondmom <- predict(mod2_glcm_secondmom, newdata=data_mean_reg_secondmom, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_secondmom <- roc(data_mean_reg_secondmom$DIAGNOSIS, test_predy_mod2_glcm_secondmom)
coords(roc_test_glcm2_secondmom, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_secondmom>0.4431727)), reference=data_mean_reg_secondmom$DIAGNOSIS)


#Combined Regression
mean_slice <- data_mean_reg$Slice_Mean
contrast_slice <- data_mean_reg_con$Slice_Mean
var_slice <- data_mean_reg_var$Slice_Mean
homog_slice <- data_mean_reg_homog$Slice_Mean
dissim_slice <- data_mean_reg_dissim$Slice_Mean
entropy_slice <- data_mean_reg_entropy$Slice_Mean
secondmom_slice <- data_mean_reg_secondmom$Slice_Mean

data_mean_reg_comb <- cbind(clin_table, mean_slice, contrast_slice, var_slice,
                            homog_slice, dissim_slice, entropy_slice, secondmom_slice)
data_mean_reg_comb$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_comb$DIAGNOSIS == 1, 0, 1))
colnames(data_mean_reg_comb)
mod2_glcm_comb <- glm(DIAGNOSIS ~ mean_slice + contrast_slice + var_slice + homog_slice + dissim_slice +
                        entropy_slice + secondmom_slice, data=data_mean_reg_comb, family="binomial")
summary(mod2_glcm_comb)
test_predy_mod2_glcm_comb <- predict(mod2_glcm_comb, newdata=data_mean_reg_comb, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_comb <- roc(data_mean_reg_comb$DIAGNOSIS, test_predy_mod2_glcm_comb)
coords(roc_test_glcm2_comb, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_comb>0.453915)), reference=data_mean_reg_comb$DIAGNOSIS)



#Combined Reg No Second Moment
#Combined Regression
set.seed(68)
mean_slice <- data_mean_reg$Slice_Mean
contrast_slice <- data_mean_reg_con$Slice_Mean
var_slice <- data_mean_reg_var$Slice_Mean
homog_slice <- data_mean_reg_homog$Slice_Mean
dissim_slice <- data_mean_reg_dissim$Slice_Mean
entropy_slice <- data_mean_reg_entropy$Slice_Mean

data_mean_reg_comb <- cbind(clin_table, mean_slice, contrast_slice, var_slice,
                            homog_slice, dissim_slice, entropy_slice)
data_mean_reg_comb$DIAGNOSIS <- as.factor(ifelse(data_mean_reg_comb$DIAGNOSIS == 1, 0, 1))
colnames(data_mean_reg_comb)
mod2_glcm_comb <- glm(DIAGNOSIS ~ mean_slice + contrast_slice + var_slice + homog_slice + dissim_slice +
                        entropy_slice, data=data_mean_reg_comb, family="binomial")
summary(mod2_glcm_comb)
test_predy_mod2_glcm_comb <- predict(mod2_glcm_comb, newdata=data_mean_reg_comb, "response")
#Find best threshold using ROC Curve
roc_test_glcm2_comb <- roc(data_mean_reg_comb$DIAGNOSIS, test_predy_mod2_glcm_comb)
coords(roc_test_glcm2_comb, "best")
confusionMatrix(as.factor(as.numeric(test_predy_mod2_glcm_comb>0.453915)), reference=data_mean_reg_comb$DIAGNOSIS)


texture_mean <- function(img, n_row, n_col){
  for (i in 1:n_row){
    for (j in 1:n_col){
      if (length(unique(raster(img[[i]][,j,]))) == 1) {
        glcm_mean_mean[i, j] <- NA
        next
      }
      
      rglcm_pt <- glcm(raster(img[[i]][,j,]), 
                       window = c(9,9), 
                       shift = c(1,1), 
                       statistics = c("mean", "variance", "homogeneity", "contrast", 
                                      "dissimilarity", "entropy", "second_moment")
      )
      glcm_mean_mean[i, j] <- sum(as.matrix(rglcm_pt$glcm_mean),
                                  na.rm = TRUE)/(length(as.matrix(rglcm_pt$glcm_mean))
                                                 - sum(is.na(as.matrix(rglcm_pt$glcm_mean))))
      
    }
  }
  return(glcm_mean_mean)
}

glcm_mean_mat <- texture_mean(roi.list, n_row, n_col)







#Test
rglcm_pt_test <- glcm(raster(roi.list[[1]][,1,]), 
                 window = c(9,9), 
                 shift = c(1,1), 
                 statistics = c("mean", "variance", "homogeneity", "contrast", 
                                "dissimilarity", "entropy", "second_moment")
)

glcm_mean_mean[1, 1] <- sum(as.matrix(rglcm_pt_test$glcm_mean),
                            na.rm = TRUE)/(length(as.matrix(rglcm_pt_test$glcm_mean))
                                           - sum(is.na(as.matrix(rglcm_pt_test$glcm_mean))))

glcm_mean_mean











rglcm_pt1 <- glcm(raster(pt1_test[,2,]), 
              window = c(9,9), 
              shift = c(1,1), 
              statistics = c("mean", "variance", "homogeneity", "contrast", 
                             "dissimilarity", "entropy", "second_moment")
)

glcm_metrics(pt1_test[,2,])

test_glcm <- glcm(raster(pt1_test[,2,]))
test_glcm$homogeneity

rglcm_pt1$glcm_homogeneity
rglcm_pt1$glcm_mean[1:882]
rglcm_pt1[,,"glcm_mean"]
plot(rglcm_pt1)




############### Test #################
test_glcm <- raster(pt1_test[,2,])
rglcm1_test <- glcm(test_glcm, 
               window = c(9,9), 
               shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
               statistics = c("mean", "variance", "homogeneity", "contrast", 
                              "dissimilarity", "entropy", "second_moment")
)

plot(rglcm1_test)
rglcm1_test
#####################################
#average across all slices for each indiv. Prob fine to to use all these at once unless highly correlated
glcm.red <- glcm(RED,
                 window = c(7, 7),
                 shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
                 statistics = c("mean",
                                "variance",
                                "homogeneity",
                                "contrast",
                                "entropy", 
                                "dissimilarity",
                                "second_moment", 
                                "correlation"))
glcm.red



### FPC Regression ###
library(dplyr)
library(fda)
library(fda.usc)

clinical
dim(t(fpca_scores))
fpca_scores_thres
fpca_scores_dir

#To assess variability in both the mean and
#the first PC, we bootstrap this procedure by resampling subjects with replacement 1000 times

fpc_dataframe <- cbind(t(fpca_scores), t(fpca_scores_thres), t(fpca_scores_dir))
dim(fpc_dataframe)
colnames(fpc_dataframe) <- c("Subject_FPC1", "Subject_FPC2", "Threshold_FPC1", "Threshold_FPC2", "Direction_FPC1", "Direction_FPC2")

fda_df <- t(final_matrix_hippo_2group)
dim(fda_df)
cn_col <- matrix("CN", 25272, 1)
dem_col <- matrix("Dementia", 18144, 1)
clinical_diag <- rbind(cn_col, dem_col)
fda_final_df <- cbind(fpc_dataframe, clinical_diag)
head(fda_final_df)
colnames(fda_final_df) <- c("Subject_FPC1", "Subject_FPC2", "Threshold_FPC1", "Threshold_FPC2", "Direction_FPC1", "Direction_FPC2", "clinical_diag")
#AGE", "MMSE
table(clinical_diag)

clinical_cn <- clinical %>% filter(DIAGNOSIS == 1)
clinical_cn[,1]
clinical_dem <- clinical %>% filter(DIAGNOSIS == 3)
clinical_dem[,1]
clinical_twogrp <- rbind(clinical_cn, clinical_dem)

MMSE_col <- sapply(1:67, function(i) rep(clinical_twogrp[i,4], 648))
mmse_column <- c(MMSE_col)
library(kableExtra)
kbl(table(c(MMSE_col)), col.names=c("MMSE", "Freq"))

#eigenvalues are the same; KX and KW are diff actually. 
#variability explained by diff lvls seem to be explaining most of variability so maybe that's why we're not gaining anything additional for the last layer
#Numerically don't understand why that's the same.
#Why exactly the same numbers? Look at full eigenvalues and see if there are any differences down the line - ani
#Poster - put first two layers and leave discussion for later
#KW is multiple of KX (maybe scaled by 8.11)

age_col <- sapply(1:67, function(i) rep(clinical_twogrp[i,3], 648))
age_column <- c(age_col)
summary(age_column)

df_clin_fda <- as.data.frame(cbind(fda_final_df, mmse_column, age_column))
dim(df_clin_fda)
str(df_clin_fda)
colnames(df_clin_fda)
head(df_clin_fda)
df_clin_fda$mmse_column <- as.numeric(df_clin_fda$mmse_column)
df_clin_fda$age_column <- as.numeric(df_clin_fda$age_column)
df_clin_fda$Subject_FPC1 <- as.numeric(df_clin_fda$Subject_FPC1)
df_clin_fda$Subject_FPC2 <- as.numeric(df_clin_fda$Subject_FPC2)
df_clin_fda$Threshold_FPC1 <- as.numeric(df_clin_fda$Threshold_FPC1)
df_clin_fda$Threshold_FPC2 <- as.numeric(df_clin_fda$Threshold_FPC2)
df_clin_fda$Direction_FPC1 <- as.numeric(df_clin_fda$Direction_FPC1)
df_clin_fda$Direction_FPC2 <- as.numeric(df_clin_fda$Direction_FPC2)
df_clin_fda$clinical_diag <- ifelse(df_clin_fda$clinical_diag == "Dementia", "1", "0")
df_clin_fda$clinical_diag <- ifelse(df_clin_fda$clinical_diag == "1", 1, 0)
table(df_clin_fda$clinical_diag)
head(df_clin_fda)

str(df_clin_fda)
fpc_reg_mod1 <- glm(clinical_diag ~ Subject_FPC1 + Subject_FPC2 + Threshold_FPC1 + Threshold_FPC2 + Direction_FPC1 + Direction_FPC2 + age_column + mmse_column, data=df_clin_fda, family="binomial")
summary(fpc_reg_mod1)

#Transform 0 to 1, try upper and lower 10th percentile for FPC1 Subj; maybe quartile
plot()
head(df_clin_fda)
fpc_reg_mod2 <- glm(clinical_diag ~ Subject_FPC1 + Subject_FPC2 + Threshold_FPC1 + Threshold_FPC2 + age_column + mmse_column, data=df_clin_fda, family="binomial")
summary(fpc_reg_mod2)
test_predy <- predict(fpc_reg_mod2, newdata=df_clin_fda, "response")
#Find best threshold using ROC Curve
roc_test <- roc(df_clin_fda$clinical_diag, test_predy)
coords(roc_test, "best")
confusionMatrix(as.factor(as.numeric(test_predy>0.445)), reference=as.factor(df_clin_fda$clinical_diag))
exp(3.493e-01)
exp(-1.614e-04)



#Exclude mmse
fpc_reg_mod_new <- glm(clinical_diag ~ Subject_FPC1 + Subject_FPC2 + Threshold_FPC1 + Threshold_FPC2 + age_column, data=df_clin_fda, family="binomial")
summary(fpc_reg_mod_new)
test_predy <- predict(fpc_reg_mod_new, newdata=df_clin_fda, "response")
#Find best threshold using ROC Curve
roc_test <- roc(df_clin_fda$clinical_diag, test_predy)
coords(roc_test, "best")
confusionMatrix(as.factor(as.numeric(test_predy>0.4244)), reference=as.factor(df_clin_fda$clinical_diag))


fpc_reg_mod3 <- lm(Subject_FPC1 ~ as.factor(clinical_diag) + age_column, data=df_clin_fda)
summary(fpc_reg_mod3)

fpc_reg_mod33 <- lm(Subject_FPC1 ~ as.factor(clinical_diag) + age_column +mmse_column, data=df_clin_fda)
summary(fpc_reg_mod33)







fpc_reg_mod4 <- glm(Subject_FPC2 ~ clinical_diag  + age_column + mmse_column, data=df_clin_fda)
summary(fpc_reg_mod4)
summary(df_clin_fda$Subject_FPC2)
length(df_clin_fda$Subject_FPC2)

fpc_reg_mod5 <- glm(Threshold_FPC1 ~ clinical_diag  + age_column + mmse_column, data=df_clin_fda)
summary(fpc_reg_mod5)
#Focus on prediction first and interpretation if we have time



#FPCA Mixed Effects
T <- seq(1, 100, by = 1)
G <- 2 #Number of Groups
L <- 28 #Number of Subjects per Group
J <- 9 #Number of Thresholds per Subject
V <- 72 #Number of Directions per Threshold

cn_ind <- rep("CN", 25272) 
ad_ind <- rep("AD", 18144)
G_ind <- c(cn_ind, ad_ind)
Subj_ind <- rep(1:67, each=648)
thresh_ind <-  rep(1:603, each = 72) 

#Mixed Model Method
library(lme4)
library(lmerTest)

sim_data <- t(scale(final_matrix_hippo_2group, center=apply(final_matrix_hippo_2group, 2, mean), scale=F))
sim_pca <- prcomp(sim_data, scale. = F)

pca_data <- data.frame(pc1 = sim_pca$x[,1], group = G_ind, subject = Subj_ind, threshold = thresh_ind)
fit <- summary(lmer(pc1~ factor(group) + (1|subject) + (1|threshold)  , data = pca_data))
fit
t_test <- fit$coefficients[2,4]; t_test
t_p <- (fit$coefficients[2,5] < 0.05); t_p


fit_diag <- glmer(factor(group)~ pc1 + (1|subject) + (1|threshold)  , data = pca_data, family="binomial") #singularity issue
fit_diag
fit_diag <- glmer(factor(group)~ pc1 + (1|subject), data = pca_data, family="binomial")
fit_diag
summary(fit_diag)


















#Concatenation of features
img_texture_concat <- function(img, n_row, n_col) {
  # Initialize an empty list to store feature vectors for all patients
  patient_features <- list()
  
  # Iterate over patients (outer loop)
  for (i in 1:n_row) {
    # Initialize an empty list to store texture features for each slice
    slice_features <- list()
    
    # Iterate over slices (inner loop)
    for (j in 1:n_col) {
      rglcm_pt <- glcm(raster(img[[i]][,j,]), 
                       window = c(9,9), 
                       shift = c(1,1), 
                       statistics = c("mean", "variance", "homogeneity", "contrast", 
                                      "dissimilarity", "entropy", "second_moment")
      )
      # Append texture features for the current slice to the list
      slice_features[[j]] <- rglcm_pt$glcm_mean
    }
    
    # Unlist and concatenate texture features for all slices into a single vector
    patient_features[[i]] <- unlist(slice_features)
  }
  
  # Return the list of feature vectors for all patients
  return(patient_features)
}




