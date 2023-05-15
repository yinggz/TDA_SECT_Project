#Tau 7
###############################################################################
load("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/ROIlist_hippo.rda")
load("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/SubjectNames_hippo.rda")

load("/Users/garyzhou/Downloads/Dropbox/parahippocampal_tau7/ROIlist_parahippocampal.rda")
load("/Users/garyzhou/Downloads/Dropbox/parahippocampal_tau7/SubjectNames_parahippocampal.rda")

library(raster)
library(cowplot)
library(ggplot2)
library(dplyr)
library(knitr)
pt1_test <- roi.list[[1]]
plot(raster(pt1_test[,2,]))
sort(pt1_test[,2,])

str(roi.list)
pt1_test <- roi.list[[1]]
plot(raster(pt1_test[,2,]))
nlevels(as.factor(pt1_test[,2,]))

#Average hyperintensity
intensity_val <- matrix(0, nrow = 159, ncol = 19404)
for (i in 1:159){
    intensity_val[i, ] <- c(roi.list[[i]])
    
}
mean_intensity_values <- apply(intensity_val, MARGIN = 1, FUN = mean)
mean_intensity_values

final_subj_eda <- plot.dat.1.29_hippo[, "PTID"]
subj_of_interest_eda <- which(subj.names %in% final_subj_eda) #subj.names froom glcm_hippo.R
subj_of_interest_eda

mean_intensity_values[subj_of_interest_eda]

#CN Mean Intensity
final_subj_cn <- plot.dat.1.29_hippo[plot.dat.1.29_hippo$DIAGNOSIS == 1, "PTID"]
subj_of_interest_cn <- which(subj.names %in% final_subj_cn)
mean(mean_intensity_values[subj_of_interest_cn])

#MCI
final_subj_mci <- plot.dat.1.29_hippo[plot.dat.1.29_hippo$DIAGNOSIS == 2, "PTID"]
subj_of_interest_mci <- which(subj.names %in% final_subj_mci)
mean(mean_intensity_values[subj_of_interest_mci])

#Dementia
final_subj_dem <- plot.dat.1.29_hippo[plot.dat.1.29_hippo$DIAGNOSIS == 3, "PTID"]
subj_of_interest_dem <- which(subj.names %in% final_subj_dem)
mean(mean_intensity_values[subj_of_interest_dem])

#plot.dat.1.29_hippo[, c("PTID", "DIAGNOSIS")]

##### SEC Values #####





numlvls <- matrix(0, nrow = 159, ncol = 22)
for (i in 1:159){
  for (j in 1:22){
    numlvls[i, j] <- nlevels(as.factor(roi.list[[i]][,j,]))
  }
}

max_values <- apply(numlvls, MARGIN = 1, FUN = max)
max_values
numlvls

numlvls[1, 1] <- nlevels(as.factor(roi.list[[1]][,1,]))


clinical



(clinical_hip_reg_fin$AGE)


eda_clinical <- clinical_hip_reg_fin 
eda_clinical <- eda_clinical %>%
  mutate(diagnosis = case_when(
    DIAGNOSIS == 1 ~ "CN",
    DIAGNOSIS == 2 ~ "MCI",
    DIAGNOSIS == 3 ~ "AD"
  ))


#Summary statistics for each covariate
summary(eda_clinical$AGE)
summary(eda_clinical$AGE)



##############################################################
#AGE: Mean and 95% CI
summary_by_diagnosis_age <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Mean = mean(AGE, na.rm = TRUE),
    `95% CI Lower` = t.test(AGE, conf.level = 0.95)$conf.int[1],
    `95% CI Upper` = t.test(AGE, conf.level = 0.95)$conf.int[2]
  )
summary_by_diagnosis_age

#MMSE: Mean and 95% CI
summary_by_diagnosis_mmse <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Mean = mean(MMSE, na.rm = TRUE),
    `95% CI Lower` = t.test(MMSE, conf.level = 0.95)$conf.int[1],
    `95% CI Upper` = t.test(MMSE, conf.level = 0.95)$conf.int[2]
  )
summary_by_diagnosis_mmse

#EDUC: Mean and 95% CI
summary_by_diagnosis_educ <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Mean = mean(PTEDUCAT, na.rm = TRUE),
    `95% CI Lower` = t.test(PTEDUCAT, conf.level = 0.95)$conf.int[1],
    `95% CI Upper` = t.test(PTEDUCAT, conf.level = 0.95)$conf.int[2]
  )
summary_by_diagnosis_educ

#Gender: Frequencies
gender_proportions_by_diagnosis <- eda_clinical %>%
  count(diagnosis, PTGENDER) %>%
  # Calculate the total count within each diagnosis group
  group_by(diagnosis) %>%
  # Calculate the proportions based on the total count
  mutate(proportion = n / sum(n)) %>%
  # Optionally, remove the frequency column if only proportions are needed
  ungroup()
gender_proportions_by_diagnosis

gender_frequencies_by_diagnosis <- eda_clinical %>%
  group_by(diagnosis, PTGENDER) %>%
  count() %>%
  mutate(proportion = n/sum(n))
gender_frequencies_by_diagnosis
###############################################################


summary_by_diagnosis_mmse <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Min = min(MMSE, na.rm = TRUE),
    `1st Qu` = quantile(MMSE, 0.25, na.rm = TRUE),
    Median = median(MMSE, na.rm = TRUE),
    Mean = mean(MMSE, na.rm = TRUE),
    `3rd Qu` = quantile(MMSE, 0.75, na.rm = TRUE),
    Max = max(MMSE, na.rm = TRUE)
  )

summary_by_diagnosis_educ <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Min = min(PTEDUCAT, na.rm = TRUE),
    `1st Qu` = quantile(PTEDUCAT, 0.25, na.rm = TRUE),
    Median = median(PTEDUCAT, na.rm = TRUE),
    Mean = mean(PTEDUCAT, na.rm = TRUE),
    `3rd Qu` = quantile(PTEDUCAT, 0.75, na.rm = TRUE),
    Max = max(PTEDUCAT, na.rm = TRUE)
  )

# Combine the tibbles into one table
combined_table <- summary_by_diagnosis_age %>%
  left_join(summary_by_diagnosis_educ, by = "diagnosis", suffix = c("_age", "_education")) %>%
  left_join(summary_by_diagnosis_mmse, by = "diagnosis") %>%
  rename_with(.cols = -diagnosis, .fn = ~paste0(., "_mmse"))

# Convert the table to LaTeX code using kable
latex_code <- kable(combined_table, format = "latex", booktabs = TRUE, align = "c")

# Print the LaTeX code
cat(latex_code)





cont_eda_clin <- eda_clinical[,c(3, 4, 6, 8)]
summary_by_diagnosis_age <- eda_clinical %>%
  group_by(diagnosis) %>%
  summarise(
    Min = min(AGE, na.rm = TRUE),
    `1st Qu` = quantile(AGE, 0.25, na.rm = TRUE),
    Median = median(AGE, na.rm = TRUE),
    Mean = mean(AGE, na.rm = TRUE),
    `3rd Qu` = quantile(AGE, 0.75, na.rm = TRUE),
    Max = max(AGE, na.rm = TRUE)
  )


summary_by_diagnosis <- cont_eda_clin %>%
  group_by(diagnosis) %>%
  summarise_all(list(summary))

#Visualize Age by Group
# Create a ggplot object with intensity values on the x-axis
p <- ggplot(eda_clinical, aes(x = AGE, fill = diagnosis)) +
  # Add histogram layer with alpha transparency for overlaid histograms
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  # Add density plot layer to show the distribution
  geom_density(alpha = 0.7) +
  # Customize the colors for each diagnosis group
  scale_fill_manual(values = c("CN" = "blue", "MCI" = "green", "AD" = "red")) +
  # Add plot labels
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Density",
       fill = "Diagnosis Group") +
  # Customize the plot theme
  theme_minimal()

# Display the plot
print(p)

pl <- ggplot(eda_clinical, aes(x = MMSE, fill = diagnosis)) +
  # Add histogram layer with alpha transparency for overlaid histograms
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  # Add density plot layer to show the distribution
  geom_density(alpha = 0.7) +
  # Customize the colors for each diagnosis group
  scale_fill_manual(values = c("CN" = "blue", "MCI" = "green", "AD" = "red")) +
  # Add plot labels
  labs(title = "Distribution of MMSE Scores",
       x = "MMSE Scores",
       y = "Density",
       fill = "Diagnosis Group") +
  # Customize the plot theme
  theme_minimal()

# Display the plot
print(pl)

pl2 <- ggplot(eda_clinical, aes(x =PTEDUCAT, fill = diagnosis)) +
  # Add histogram layer with alpha transparency for overlaid histograms
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  # Add density plot layer to show the distribution
  geom_density(alpha = 0.7) +
  # Customize the colors for each diagnosis group
  scale_fill_manual(values = c("CN" = "blue", "MCI" = "green", "AD" = "red")) +
  # Add plot labels
  labs(title = "Distribution of Education Level",
       x = "Education Level (Years)",
       y = "Density",
       fill = "Diagnosis Group") +
  # Customize the plot theme
  theme_minimal()

# Display the plot
print(pl2)

combined_plot <- plot_grid(p, pl, pl2, nrow=1)
print(combined_plot)

clinical_hip_reg_fin$

non_zero_data <- 

#Visualize Age by Group
# Create a ggplot object with intensity values on the x-axis
p <- ggplot(non_zero_data, aes(x = intensity, fill = diagnosis)) +
  # Add histogram layer with alpha transparency for overlaid histograms
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  # Add density plot layer to show the distribution
  geom_density(alpha = 0.7) +
  # Customize the colors for each diagnosis group
  scale_fill_manual(values = c("CN" = "blue", "MCI" = "green", "AD" = "red")) +
  # Add plot labels
  labs(title = "Distribution of Voxel Intensity Values",
       x = "Voxel Intensity",
       y = "Density",
       fill = "Diagnosis Group") +
  # Customize the plot theme
  theme_minimal()

# Display the plot
print(p)



