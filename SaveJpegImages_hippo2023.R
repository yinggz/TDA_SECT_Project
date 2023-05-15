install.packages("raster")
library(raster)

#Tau 7
###############################################################################
load("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/ROIlist_hippo.rda")
load("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/SubjectNames_hippo.rda")

#Test
options(max.print=1300)
pt1_test <- roi.list[[1]]
plot(raster(pt1_test[,2,]))
sort(pt1_test[,2,])


#max intensity is 4.195
max(sapply(1:159, function(x) max(roi.list[[x]])))
#lowest max val is 0.79
min(sapply(1:159, function(x) max(roi.list[[x]]))) 
sort(sapply(1:159, function(x) max(roi.list[[x]])))
hist(sort(sapply(1:159, function(x) max(roi.list[[x]]))), main="Distrib of Max Intensities")
summary(sort(sapply(1:159, function(x) max(roi.list[[x]]))))

#min intensity of 0 for all
sapply(1:159, function(x) min(roi.list[[x]])) 

#mean intensity ranges from 0.0076 to 0.218 w/ mean 0.1
sort(sapply(1:159, function(x) mean(roi.list[[x]]))) #min is 0.0076, 
summary(sapply(1:159, function(x) mean(roi.list[[x]])))

#median intensity of 0 for all
sapply(1:159, function(x) median(roi.list[[x]])) 

test_matr <- sapply(1:159, function(x) c(roi.list[[x]]))
unique_val <- apply(test_matr, 2, unique)
unique_valnozero <- rep(0, 159)
for (i in 1:159){
  unique_valnozero[i] <- min(unique_val[[i]][-1])
}
options(scipen=999) 
min_nonzero_val <- unique_valnozero
max_nonzero_val <- sapply(1:159, function(x) max(roi.list[[x]]))
#Diff b/w max and min intensity values
summary(max_nonzero_val - min_nonzero_val)

plot(min_nonzero_val, max_nonzero_val, xlab="Minimum Nonzero Intensity", ylab="Maximum Nonzero Intensity", pch=20)
plot(min_nonzero_val, max_nonzero_val - min_nonzero_val, xlab="Minimum Nonzero Intensity", ylab="Range of Nonzero Intensity", pch=20)
cor(min_nonzero_val, max_nonzero_val - min_nonzero_val)
hist(max_nonzero_val, main="Hist of Max Non-zero")
hist(min_nonzero_val, main="Hist of Min Non-zero")

### Method 1: Based on Min and Max
#Define threshold
num_thres <- 10
#Get range of nonzero values for each patient
range_nonzero_val <- max_nonzero_val - min_nonzero_val
summary(range_nonzero_val)
#Take the maximum range, find the corresponding patient, and find the min intensity lvl
min_val_max_range <- min_nonzero_val[which(range_nonzero_val == max(range_nonzero_val))]
#max_nonzero_val[which(range_nonzero_val == max(range_nonzero_val))]
#Evenly divide the range for that patient by number of thresholds 
sapply(1:(num_thres-1), function(x) max(range_nonzero_val)*x/num_thres + min_val_max_range)

### Method 2: Based on IQR
#Define threshold
num_thres <- 10
#Get min 1st quartile IQR and max 3rd quartile IQR among all patients
nonzero_vals <- sapply(1:159, function(x) roi.list[[x]][roi.list[[x]] != 0])
#Summary of Number of Nonzero Voxels Per Patient
sort(sapply(1:159, function(x) length(nonzero_vals[[x]])))
hist(sort(sapply(1:159, function(x) length(nonzero_vals[[x]]))), main="Distrib of Number of Nonzero Voxels Per Patient")
summary(sort(sapply(1:159, function(x) length(nonzero_vals[[x]]))))
#Summary Statistics for Each Patient's Scans (Non-zero)
sapply(1:159, function(x) summary(nonzero_vals[[x]]))
#Get IQR for all patients (Non-zero)
nonzero_1st_quart <- summary(sort(sapply(1:159, function(x) summary(nonzero_vals[[x]]))))[2]
nonzero_3rd_quart <- summary(sort(sapply(1:159, function(x) summary(nonzero_vals[[x]]))))[5]
nonzero_iqr <- nonzero_3rd_quart - nonzero_1st_quart
#Evenly divide IQR by number of thresholds 
threshold_val <- unname(sapply(1:(num_thres-1), function(x) nonzero_iqr*x/num_thres + nonzero_1st_quart))


#1.3672 to 1.8911
threshold_val
num_thres
nonzero_iqr

#Cutoff 1: 1.044823
q=threshold_val[1]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.04/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 2: 1.086426
q = threshold_val[2]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.09/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 3: 1.128028
q=threshold_val[3]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.13/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 4: 1.169630
q=threshold_val[4]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.17/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 5: 1.211232
q=threshold_val[5]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.21/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 6: 1.252834
q=threshold_val[6]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.25/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 7: 1.294437
q=threshold_val[7]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.29/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 8: 1.336039
q=threshold_val[8]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.34/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}

################################################################################################
#Cutoff 9: 1.377641
q=threshold_val[9]
for(subj in 1:length(subj.names)){
  subj.image = roi.list[[subj]]
  mask = subj.image
  mask[subj.image > q] = 1
  mask[subj.image <= q] = 0
  setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.38/images")
  dir.create(subj.names[subj])
  setwd(subj.names[subj])
  for(i in 1:(dim(mask)[2])){
    img = mask[,i,]
    img1 = matrix(0, dim(img)[1] + 15, dim(img)[2] + 10)
    img1[10:(9+dim(img)[1]), 5:(4+dim(img)[2])] = img
    fname = paste(i, "image.png", sep = "")
    png(fname)
    par(mar=c(0,0,0,0))
    image(img1, axes = FALSE, col=gray.colors(2))
    dev.off()
  }
  print(subj)
}




###Plot Distributions by Diagnosis Groups###
clinical
ECs.dat[1:20,1:7]
plot.dat = merge(clinical, ECs.dat, by = "PTID") 