#setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7")
#####################################################################################################################################
### Separate Diagnosis Hypothesis Test: AD and CN
#Dementia (N=28)
# Define the numeric values to iterate over
numeric_values <- c(0.95, 0.97, 0.98, "1.00", 1.01, 1.03, 1.04, 1.06,
                    1.07, 1.09, "1.10", 1.12, 1.13, 1.15, 1.16, 1.18,
                    1.19, 1.21, 1.22, 1.24, 1.25, 1.27, 1.28, "1.30",
                    1.31, 1.33, 1.34, 1.36, 1.37, 1.39)

numeric_values <- c(0.95, 0.98, 1.01, 1.04,
                    1.07, 1.13, 1.16, 1.18, 1.21, 
                    1.24, 1.27, 
                    "1.30", 1.33, 1.36, 1.39)

# Create a list to store the filtered data frames
dem_list <- list()

# Loop over the numeric values
for (val in numeric_values) {
  # Create the variable names dynamically
  plot_var_name <- paste0("plot.dat.", val, "_hippo")
  dem_var_name <- paste0("dem", val, "_hippo")
  
  # Filter the data based on DIAGNOSIS == 3 and complete cases
  DEM_hippo <- get(plot_var_name)[get(plot_var_name)$DIAGNOSIS == 3, 6:7205]
  dem_hippo <- DEM_hippo[complete.cases(DEM_hippo), ]
  
  # Store the filtered data frame in the list
  dem_list[[dem_var_name]] <- dem_hippo
}

# Convert the list of data frames to individual variables in the global environment
list2env(dem_list, envir = .GlobalEnv)


#Reformat dataset
# Create lists to store the transposed data frames, vectors, and matrices
t_dem_list <- list()
dem_vector_list <- list()
dem_matrix_list <- list()

# Loop over the numeric values
for (val in numeric_values) {
  # Create the variable names dynamically
  dem_var_name <- paste0("dem", val, "_hippo")
  t_dem_var_name <- paste0("t_dem.", val, "_hippo")
  dem_vector_var_name <- paste0("dem_vector_", val, "_hippo")
  dem_matrix_var_name <- paste0("dem_matrix_", val, "_hippo")
  
  # Perform the necessary operations
  t_dem_hippo <- t(get(dem_var_name))
  dem_vector_hippo <- c(t_dem_hippo)
  dim(dem_vector_hippo) <- c(100, 2016)
  dem_matrix_hippo <- dem_vector_hippo
  
  # Store the results in the lists
  t_dem_list[[t_dem_var_name]] <- t_dem_hippo
  dem_vector_list[[dem_vector_var_name]] <- dem_vector_hippo
  dem_matrix_list[[dem_matrix_var_name]] <- dem_matrix_hippo
}

# Convert the lists to individual variables in the global environment
list2env(t_dem_list, envir = .GlobalEnv)
list2env(dem_vector_list, envir = .GlobalEnv)
list2env(dem_matrix_list, envir = .GlobalEnv)

# Define the numeric values to iterate over and the corresponding ranges
numeric_values <- c("095", "098", "101", "104",
                    "107", "113", "116", "118",
                    "121", "124", "127", "130",
                    "133", "136", "139")

# Function to generate ranges based on the index (i) and range length (len)
get_range <- function(i, len) {
  start <- (i - 1) * len + 1
  end <- start + len - 1
  return(c(start, end))
}

# Create a named list to store the vectors
mat_list <- list()

# Loop over the numeric values
for (i in seq_along(numeric_values)) {
  val <- numeric_values[i]
  
  # Get the appropriate range for the current index
  range <- get_range(i, 2016)
  
  # Create the variable name dynamically
  mat_var_name <- paste0("mat", val)
  
  # Create the vector for the specified range
  mat_vec <- c(range[1]:range[2])
  
  # Store the vector in the named list
  mat_list[[mat_var_name]] <- mat_vec
}

# Convert the named list to individual variables in the global environment
list2env(mat_list, envir = .GlobalEnv)




#############################################

# Define the block size
block_size <- 72

# Initialize an empty vector to store the concatenated values
mat_index <- c()

# Calculate the number of iterations needed based on the length of the first mat vector
num_iterations <- length(get("mat095")) %/% block_size

# Loop over the iterations to extract blocks of 72 elements
for (iter in 1:num_iterations) {
  # Calculate the start and end indices for the current block
  start_idx <- (iter - 1) * block_size + 1
  end_idx <- start_idx + block_size - 1
  
  # Loop over the numeric values and extract the current block of elements from each mat vector
  for (val in numeric_values) {
    # Get the mat vector based on the numeric value
    mat_vec <- get(paste0("mat", val))
    
    # Extract the current block of elements and concatenate to mat_index
    mat_index <- c(mat_index, mat_vec[start_idx:end_idx])
  }
}

#########CN#########
numeric_values <- c(0.95, 0.98, 1.01, 1.04,
                    1.07, 1.13, 1.16, 1.18, 1.21, 
                    1.24, 1.27, 
                    "1.30", 1.33, 1.36, 1.39)

# Create a list to store the filtered data frames
cn_list <- list()

# Loop over the numeric values
for (val in numeric_values) {
  # Create the variable names dynamically
  plot_var_name <- paste0("plot.dat.", val, "_hippo")
  cn_var_name <- paste0("cn", val, "_hippo")
  
  # Filter the data based on DIAGNOSIS == 3 and complete cases
  CN_hippo <- get(plot_var_name)[get(plot_var_name)$DIAGNOSIS == 1, 6:7205]
  cn_hippo <- CN_hippo[complete.cases(CN_hippo), ]
  
  # Store the filtered data frame in the list
  cn_list[[cn_var_name]] <- cn_hippo
}

# Convert the list of data frames to individual variables in the global environment
list2env(cn_list, envir = .GlobalEnv)


# Create lists to store the transposed data frames, vectors, and matrices
t_cn_list <- list()
cn_vector_list <- list()
cn_matrix_list <- list()

# Loop over the numeric values
for (val in numeric_values) {
  # Create the variable names dynamically
  cn_var_name <- paste0("cn", val, "_hippo")
  t_cn_var_name <- paste0("t_cn.", val, "_hippo")
  cn_vector_var_name <- paste0("cn_vector_", val, "_hippo")
  cn_matrix_var_name <- paste0("cn_matrix_", val, "_hippo")
  
  # Perform the necessary operations
  t_cn_hippo <- t(get(cn_var_name))
  cn_vector_hippo <- c(t_cn_hippo)
  dim(cn_vector_hippo) <- c(100, 2808)
  cn_matrix_hippo <- cn_vector_hippo
  
  # Store the results in the lists
  t_cn_list[[t_cn_var_name]] <- t_cn_hippo
  cn_vector_list[[cn_vector_var_name]] <- cn_vector_hippo
  cn_matrix_list[[cn_matrix_var_name]] <- cn_matrix_hippo
}

# Convert the lists to individual variables in the global environment
list2env(t_cn_list, envir = .GlobalEnv)
list2env(cn_vector_list, envir = .GlobalEnv)
list2env(cn_matrix_list, envir = .GlobalEnv)

###################
#Generate matrix index with direction as bottom layer, threshold in middle, subject at top
# Define the numeric values to iterate over and the corresponding ranges
numeric_values <- c("095", "098", "101", "104",
                    "107", "113", "116", "118",
                    "121", "124", "127", "130",
                    "133", "136", "139")

# Function to generate ranges based on the index (i) and range length (len)
get_range <- function(i, len) {
  start <- (i - 1) * len + 1
  end <- start + len - 1
  return(c(start, end))
}

# Create a named list to store the vectors
mat_list_cn <- list()

# Loop over the numeric values
for (i in seq_along(numeric_values)) {
  val <- numeric_values[i]
  
  # Get the appropriate range for the current index
  range <- get_range(i, 2808)
  
  # Create the variable name dynamically
  mat_var_name <- paste0("mat", val, "_cn")
  
  # Create the vector for the specified range
  mat_vec <- c(range[1]:range[2])
  
  # Store the vector in the named list
  mat_list_cn[[mat_var_name]] <- mat_vec
}

# Convert the named list to individual variables in the global environment
list2env(mat_list_cn, envir = .GlobalEnv)


#############################################
numeric_values <- c("095", "098", "101", "104",
                    "107", "113", "116", "118",
                    "121", "124", "127", "130",
                    "133", "136", "139")

# Define the block size
block_size <- 72

# Initialize an empty vector to store the concatenated values
mat_index_cn <- c()

# Calculate the number of iterations needed based on the length of the first mat vector
num_iterations <- length(get("mat095_cn")) %/% block_size

# Loop over the iterations to extract blocks of 72 elements
for (iter in 1:num_iterations) {
  # Calculate the start and end indices for the current block
  start_idx <- (iter - 1) * block_size + 1
  end_idx <- start_idx + block_size - 1
  
  # Loop over the numeric values and extract the current block of elements from each mat vector
  for (val in numeric_values) {
    # Get the mat vector based on the numeric value
    mat_vec <- get(paste0("mat", val, "_cn"))
    
    # Extract the current block of elements and concatenate to mat_index
    mat_index_cn <- c(mat_index_cn, mat_vec[start_idx:end_idx])
  }
}

# mat_index now contains the concatenated blocks of 72 elements from each mat vector

###############################################################
#Final Matrix Concatenate
numeric_values_decimal <- c(0.95, 0.98, 1.01, 1.04,
                                              1.07, 1.13, 1.16, 1.18, 1.21, 
                                              1.24, 1.27, 
                                              "1.30", 1.33, 1.36, 1.39)# Create a list of variable names based on the numeric values
dem_var_names <- paste0("dem_matrix_", numeric_values_decimal, "_hippo")

# Use mget to get the corresponding matrices in a list
dem_matrices <- mget(dem_var_names)

# Use do.call with cbind to concatenate all the matrices column-wise into final_mat_cn
final_mat_dem <- do.call(cbind, dem_matrices)
final_matrix_dem <- final_mat_dem[, mat_index]

# Create a list of variable names based on the numeric values
cn_var_names <- paste0("cn_matrix_", numeric_values_decimal, "_hippo")

# Use mget to get the corresponding matrices in a list
cn_matrices <- mget(cn_var_names)

# Use do.call with cbind to concatenate all the matrices column-wise into final_mat_cn
final_mat_cn <- do.call(cbind, cn_matrices)
final_matrix_cn <- final_mat_cn[, mat_index_cn]
# final_mat_cn now contains the concatenated matrices for all cn_matrix variables

# Combine the final_matrix_cn and final_matrix_dem column-wise
final_matrix <- as.matrix(cbind(final_matrix_cn, final_matrix_dem))

# Display the dimensions of the final_matrix
dim(final_matrix)


final_matrix_hip_t15 <- final_matrix
setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/thres_30")
save(final_matrix_hip_t30, file = "final_matrix_hip_t30.rda")


#Mean Function Regression
library(tidyr)
library(dplyr)
library(lme4)
library(pROC)
library(glmnet)

#Mean func for each subj
# Define the number of columns to take the mean over
group_size <- 1080
# Reshape the matrix into a 3D array where each slice represents a group of 648 columns
reshaped_array <- array(final_matrix_hip_t15, dim = c(nrow(final_matrix_hip_t15), group_size, ncol(final_matrix_hip_t15)/group_size))
# Use apply to calculate the mean of every 2160 columns along the third dimension of the array
result_col <- apply(reshaped_array, c(1,3), mean)






#Mean func as time for each patient (1 stepsize 1 column)
result_col
lasso_mean_func_df <- as.data.frame(t(result_col))
# Step 2: Create a vector of column names as "time1", "time2", ..., "time67"
column_names_las <- paste0("time", seq(1, ncol(lasso_mean_func_df)))
# Step 3: Set the column names of the data frame
colnames(lasso_mean_func_df) <- column_names_las
colnames(lasso_mean_func_df)
clin_subset <- subset(clinical_hip_reg_fin, DIAGNOSIS != 2)
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
summary(mod1_glcm) #Time 51
test_predy_mod1_glcm <- predict(mod1_glcm, newdata=lasso_mfunc, "response")
#Find best threshold using ROC Curve
roc_test_glcm <- roc(lasso_mfunc$diagnosis, test_predy_mod1_glcm)
auc(lasso_mfunc$diagnosis, test_predy_mod1_glcm)
coord_test <- coords(roc_test_glcm, "local maxima")
coord_test$specificity > 0.6
confusionMatrix(as.factor(as.numeric(test_predy_mod1_glcm>0.4599594)), reference=as.factor(lasso_mfunc$diagnosis))

lasso_mfunc$diagnosis <- as.factor(ifelse(lasso_mfunc$diagnosis == 0, "CN", "AD"))
#Cross-validation
# Define the cross-validation method (k-fold cross-validation) and use the twoClassSummary function
control <- trainControl(method = "cv",
                        number = 10,                      # 10-fold cross-validation
                        classProbs = TRUE,               # Required for twoClassSummary
                        summaryFunction = twoClassSummary) # Use twoClassSummary to get AUC

# Train the logistic regression model using cross-validation
model_t20_glcm <- train(diagnosis ~ time51, data = lasso_mfunc, method = "glm",
               family = binomial(link = "logit"), trControl = control,
               metric="ROC")

# Print the model
print(model_t20_glcm)

#Mean AUC
mean_auc <- model_t20_glcm$results$ROC

#SD of AUC
auc_std_dev <- model_t20_glcm$results$ROCSD

# Print the cross-validation results
print((model_t20_glcm$results)


lasso_mod2_t9 <- glm(diagnosis ~ time7 + AGE + as.factor(PTGENDER) + PTEDUCAT, data=lasso_mfunc, family="binomial")
summary(lasso_mod2_t9)
best_model_t9 <- step(lasso_mod2_t9, direction="backward", trace=0)
best_model_t9
predy_best_model_t9 <- predict(lasso_mod2_t9, newdata=lasso_mfunc, "response")
#Find best threshold using ROC Curve
roc_test_t9 <- roc(lasso_mfunc$diagnosis, predy_best_model_t9)
auc(lasso_mfunc$diagnosis, predy_best_model_t9)
coord_test <- coords(roc_test_glcm, "local maxima")
coords(roc_test_glcm, "best")
coord_test$specificity > 0.6
confusionMatrix(as.factor(as.numeric(predy_best_model_t9>0.4599594)), reference=as.factor(lasso_mfunc$diagnosis))















# Create predefined 10-fold cross-validation indices using createFolds
set.seed(60) 
n_folds <- createFolds(lasso_mfunc_t20$diagnosis, k = 10) #10 folds

tr_control <- trainControl(method = "cv", 
                        index = n_folds,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

#Num Threshold = 20
mod_t20 <- train(diagnosis ~ time51, data = lasso_mfunc_t20, method = "glm",
                family = binomial(link = "logit"), 
                trControl = tr_control,
                metric = "ROC")

#Num Threshold = 30
mod_t30 <- train(diagnosis ~ time7 + time51, data = lasso_mfunc_t30, method = "glm",
                family = binomial(link = "logit"),
                trControl = tr_control,
                metric = "ROC")

#Mean AUC
mean_auc_t20 <- mod_t20$results$ROC
#SD of AUC
auc_sd_t20 <- mod_t20$results$ROCSD

#Mean AUC
mean_auc_t30 <- mod_t30$results$ROC
#SD of AUC
auc_sd_t30 <- mod_t30$results$ROCSD


# Additional models can also use the same 'control' object with predefined cross-validation folds

# Print the cross-validation results for each model
print(model1$results)
print(model2$results)
