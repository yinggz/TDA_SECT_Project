library(tidyr)
library(dplyr)
library(lme4)
library(pROC)
library(glmnet)

#Mean func as time for each patient (1 stepsize 1 column)
result_col
lasso_mean_func_df <- as.data.frame(t(result_col))
# Step 2: Create a vector of column names as "time1", "time2", ..., "time67"
column_names_las <- paste0("time", seq(1, ncol(lasso_mean_func_df)))
# Step 3: Set the column names of the data frame
colnames(lasso_mean_func_df) <- column_names_las
colnames(lasso_mean_func_df)
cn_df_las <- clin_subset[clin_subset$DIAGNOSIS == 1, ]
dem_df_las <- clin_subset[clin_subset$DIAGNOSIS == 3, ]
lasso_mfunc <- data.frame(rbind(cn_df_las, dem_df_las), lasso_mean_func_df)

#LASSO Regression#
set.seed(60)
slice_matrix <- as.matrix(lasso_mfunc[,column_names_las])
diagnosis_vector <- as.vector(lasso_mfunc$DIAGNOSIS)
# Fit the LASSO model
fit <- cv.glmnet(x=slice_matrix, y=diagnosis_vector, family="binomial", type.measure="mse", alpha=1, nfolds=5)
lambda_min <- fit$lambda.min # lambda value with minimum mean cross-validated error
lambda_1se <- fit$lambda.1se
# Select the slice columns to include as variables in the regression
selected_slices <- predict(fit, type="coefficients", s=fit$lambda.min)
best_perform <- coef(fit, s=lambda_min)
vars_include <- names(best_perform[which(best_perform != 0),])[-1]
reg_pred_name <- as.formula(paste("diagnosis ~", paste(vars_include, collapse = " + ")))
lasso_mfunc$diagnosis <- ifelse(lasso_mfunc$DIAGNOSIS == 1, 0, 1)
#Redo GLCM_Reg with LASSO Variable Selection
mod1_glcm <- glm(reg_pred_name, data=lasso_mfunc, family="binomial")
summary(mod1_glcm) #Time 7
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=lasso_mfunc, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(lasso_mfunc$diagnosis, test_predy_mod1_glcm)
auc(lasso_mfunc$diagnosis, test_predy_mod1_glcm)
coords(roc_test_glcm, "best")
#confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.2887359)), reference=data_mean_reg$DIAGNOSIS)
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4435173)), reference=as.factor(lasso_mfunc$diagnosis))

lasso_mod2_t9 <- glm(diagnosis ~ time7 + AGE + as.factor(PTGENDER) + PTEDUCAT, data=lasso_mfunc, family="binomial")
summary(lasso_mod2_t9)
best_model_t9 <- step(lasso_mod2_t9, direction="backward", trace=0)
best_model_t9
predy_best_model_t9 <- predict(lasso_mod2_t9, newdata=lasso_mfunc, "response")
#Find best threshold using ROC Curve
roc_test_t9 <- roc(lasso_mfunc$diagnosis, predy_best_model_t9)
auc(lasso_mfunc$diagnosis, predy_best_model_t9)
coords(roc_test_t9, "best")
confusionMatrix(as.factor(as.numeric(predy_best_model_t9>0.4145637)), reference=as.factor(lasso_mfunc$diagnosis))


#Mean func for each subj
# Define the number of columns to take the mean over
group_size <- 648
# Reshape the matrix into a 3D array where each slice represents a group of 648 columns
reshaped_array <- array(final_matrix_hippo_2group, dim = c(nrow(final_matrix_hippo_2group), group_size, ncol(final_matrix_hippo_2group)/group_size))
# Use apply to calculate the mean of every 648 columns along the third dimension of the array
result_col <- apply(reshaped_array, c(1,3), mean)

###Attempt 2
num_time_points <- nrow(mean_matrix)
num_individuals <- ncol(mean_matrix)
mean_matrix_long <- as.data.frame(t(mean_matrix))
mean_matrix_long$individual <- 1:num_individuals
mean_matrix_long$group <- c(rep("CN", 39), rep("AD", 28))

# Convert the matrix to a long-format data frame
mean_matrix_long <- gather(mean_matrix_long, key = "time", value = "matrix_value", -c(individual, group))

# Expand the clinical data frame by duplicating each row 100 times
# Assume your clinical data frame is called 'clinical_data' and has dimensions 67x6
clinical_data_expanded <- clin_subset[rep(row.names(clin_subset), each = num_time_points), ]

# Combine the expanded clinical data frame with the long-format matrix data
combined_data <- bind_cols(clinical_data_expanded, select(mean_matrix_long, matrix_value))
combined_data$diagnosis <- ifelse(combined_data$DIAGNOSIS == 1, 0, 1)
combined_data$stepsize <- rep(1:100, 67)

me_model1 <- glmer(diagnosis ~ matrix_value + (1 | PTID),
                  data = combined_data, family = binomial(link = "logit"))
summary(me_model1)

me_model2 <- glmer(diagnosis ~ matrix_value + (1 + matrix_value| PTID),
                   data = combined_data, family = binomial(link = "logit"))
summary(me_model2)

anova(me_model1, me_model2)

me_model3 <- glmer(diagnosis ~ matrix_value + PTEDUCAT + AGE + as.factor(PTGENDER) + (1 + matrix_value| PTID),
                   data = combined_data, family = binomial(link = "logit"))
summary(me_model3)

anova(me_model1, me_model3)




# Obtain predicted probabilities from the model
predicted_probabilities <- predict(me_model1, type = "response")
# Create an ROC curve
roc_obj <- roc(combined_data$diagnosis, predicted_probabilities)
# Calculate AUC
auc(roc_obj)
# Alternatively, you can choose the threshold that maximizes the sum of sensitivity and specificity
coords(roc_obj, "best", ret = c("sensitivity", "specificity"), transpose = FALSE, best.method = "closest.topleft")







# Load the tidyverse package
library(tidyverse)

# Assume your matrix with mean functions is called 'mean_matrix' and has dimensions 100x67
# Step 1: Convert the matrix to a long-format data frame
mean_matrix_long <- as.data.frame(t(mean_matrix))
colnames(mean_matrix_long) <- paste0("time_", 1:ncol(mean_matrix_long))
mean_matrix_long <- gather(mean_matrix_long, key = "time", value = "matrix_value", -c(individual, group))

# Step 2: Expand the clinical data frame by duplicating each row 100 times
# Assume your clinical data frame is called 'clinical_data' and has dimensions 67x6
clinical_data_expanded <- clinical[rep(row.names(clinical), each = 100), ]

# Step 3: Combine the expanded clinical data frame with the long-format matrix data
combined_data <- bind_cols(clinical_data_expanded, select(mean_matrix_long, matrix_value))

# The 'combined_data' data frame is now ready for analysis with mixed-effects logistic regression

###

###Attempt 1 Failed
mean_matrix <- result_col
mean_matrix_long <- as.data.frame(t(mean_matrix))
colnames(mean_matrix_long) <- paste0("time_", 1:ncol(mean_matrix_long))
mean_matrix_long$individual <- 1:nrow(mean_matrix_long)

# Add a "group" variable indicating CN or AD group membership
mean_matrix_long$group <- c(rep("CN", 39), rep("AD", 28))

clinical_hip_df <- rbind(clinical_hip_reg_fin[clinical_hip_reg_fin$DIAGNOSIS == 1,], clinical_hip_reg_fin[clinical_hip_reg_fin$DIAGNOSIS == 3,])
mean_matrix_long <- mean_matrix_long[order(match(mean_matrix_long$individual, clinical_data$individual)), ]

# Step 3: Combine the long-format data frame with the clinical data frame
# Assume your clinical data frame is called 'clinical_data' and has dimensions 67x6
combined_data <- cbind(clinical_data, mean_matrix_long)
###


#Overall mean func
mean_function
dim(final_matrix_hippo_2group) #Load rda file
mean_func_cn <- apply(final_matrix_hippo_2group[,1:25272], 1, mean)
mean_func_dem <- apply(final_matrix_hippo_2group[,25273:43416], 1, mean)

summary(mean_func_cn)
summary(mean_func_dem)

plot(1:100, mean_func_cn, type="l", col = "green")
lines(1:100, mean_func_dem, col="red")

#Reshape long
clin_subset <- subset(clinical_hip_reg_fin, DIAGNOSIS != 2)
mean_func_df <- data.frame(time = 1:100, mean_func_cn, mean_func_dem)
# Add a temporary key column to both data frames
clin_subset$key <- 1
mean_func_df$key <- 1
# Perform the cross join using the temporary key column
data_long <- merge(clin_subset, mean_func_df, by = "key")
# Remove the temporary key column
data_long$key <- NULL
# Add a new column for time-varying mean values based on diagnosis group
data_long$mean_function <- ifelse(data_long$DIAGNOSIS == 1, data_long$mean_func_cn, data_long$mean_func_dem)
mean_function_df_final <- data_long[,-c(9, 10)]
head(mean_function_df_final)

mean_function_df_final$diagnosis <- ifelse(mean_function_df_final$DIAGNOSIS == 1, 0, 1)
#Fit the mixed effects logistic regression model
#rand intercept for each participant
mean_function_df_final$time
me_model <- glmer(diagnosis ~ mean_function + (1 | PTID),
                   data = mean_function_df_final, family = binomial(link = "logit"))
summary(me_model)


# Obtain predicted probabilities from the model
predicted_probabilities <- predict(me_model, type = "response")
# Create an ROC curve
roc_obj <- roc(mean_function_df_final$diagnosis, predicted_probabilities)
# Calculate AUC
auc(roc_obj)
# Alternatively, you can choose the threshold that maximizes the sum of sensitivity and specificity
coords(roc_obj, "best", ret = c("sensitivity", "specificity"), transpose = FALSE, best.method = "closest.topleft")

