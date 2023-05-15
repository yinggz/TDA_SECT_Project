### Load packages

library(oro.nifti)


#MRI Tau 7
dat.dir = "/Users/garyzhou/Downloads/Dropbox/MRI_TAU7"
aal = readNIfTI("/Users/garyzhou/Downloads/Dropbox/atlas/AAL2.nii")
dim(aal)

### Extract the ROI 4101+4102 from the AAL atlas corresponding to Hippocampus
ind = which(aal == 4101, arr.ind = TRUE)
ind = rbind(ind, which(aal == 4102, arr.ind = TRUE))

### Create a 3D box where the ROI will be saved
ROI = array(0, c(max(ind[,1])-min(ind[,1])+2, 
                 max(ind[,2])-min(ind[,2])+2,
                 max(ind[,3])-min(ind[,3])+2))
#Gary Question: Why add 2 instead of 1?

### Indices in the ROI 3D box corresponding to those in native space
ind1 = ind
ind1[,1] = ind[,1] - min(ind[,1])+1
ind1[,2] = ind[,2] - min(ind[,2])+1
ind1[,3] = ind[,3] - min(ind[,3])+1

### Obtain the paths to the PET scan for each subject
subj.dat = dir(dat.dir, pattern = "*", full.names = TRUE)

### Obtain the subject IDs from the file names
subj.names = dir(dat.dir, pattern = "*", full.names = FALSE)

### A list that will contain the 3D boxes of the ROI extracted from all subjects
roi.list = c()

### Extract the ROI for each subject
for(subj in 1:length(subj.dat)){
  
  ### Obtain the file name from the subject folder
  file.name = dir(subj.dat[subj], pattern = "*", full.names = TRUE)[1]
  
  ### Load the dataset for a subject
  dat = readNIfTI(file.name)
  
  ### Extract the region
  ROI[ind1] = dat[ind]
  roi.list[[subj]] = ROI
  print(subj)
  
}

###############################################################################
setwd("/Users/garyzhou/Downloads/Dropbox/hippo_tau7")
save(roi.list, file = "ROIlist_hippo.rda")
save(subj.names, file = "SubjectNames_hippo.rda")

################################################################################################################################################################

### Specify the path to data directory
#dat.dir = "/Users/ani/Dropbox/2021TDATexture/ADNIsubset"
##dat.dir = "/home/Projects/aeloyan-projects/ADNI/RegisteredTAU"
#dat.dir = "/Users/garyzhou/Downloads/Dropbox/ADNIsubset"
dat.dir = "/Users/garyzhou/Downloads/Dropbox/ADNIfull"

### Load the AAL mask and extract the ROI
#aal = readNIfTI("/Users/ani/Dropbox/2021TDATexture/atlas/AAL2.nii")
##aal = readNIfTI("~aeloyan/ADNI/AAL2.nii")
aal = readNIfTI("/Users/garyzhou/Downloads/Dropbox/atlas/AAL2.nii")
dim(aal)

################################################################################
### Extract the ROI 4201+4202 from the AAL atlas corresponding to Amygdala
ind = which(aal == 4201, arr.ind = TRUE)
str(ind)
ind = rbind(ind, which(aal == 4202, arr.ind = TRUE))
################################################################################
### Extract the ROI 4101+4102 from the AAL atlas corresponding to Hippocampus
ind = which(aal == 4101, arr.ind = TRUE)
ind = rbind(ind, which(aal == 4102, arr.ind = TRUE))
################################################################################
### Extract the ROI 2001+2002 from the AAL atlas corresponding to Precentral
ind = which(aal == 2001, arr.ind = TRUE)
ind = rbind(ind, which(aal == 2002, arr.ind = TRUE))
################################################################################
### Extract the ROI 2501+2502 from the AAL atlas corresponding to Olfactory
ind = which(aal == 2501, arr.ind = TRUE)
ind = rbind(ind, which(aal == 2502, arr.ind = TRUE))

### Create a 3D box where the ROI will be saved
ROI = array(0, c(max(ind[,1])-min(ind[,1])+2, 
                 max(ind[,2])-min(ind[,2])+2,
                 max(ind[,3])-min(ind[,3])+2))
#Gary Question: Why add 2 instead of 1?

### Indices in the ROI 3D box corresponding to those in native space
ind1 = ind
ind1[,1] = ind[,1] - min(ind[,1])+1
ind1[,2] = ind[,2] - min(ind[,2])+1
ind1[,3] = ind[,3] - min(ind[,3])+1

### Obtain the paths to the PET scan for each subject
subj.dat = dir(dat.dir, pattern = "*", full.names = TRUE)

### Obtain the subject IDs from the file names
subj.names = dir(dat.dir, pattern = "*", full.names = FALSE)

### A list that will contain the 3D boxes of the ROI extracted from all subjects
roi.list = c()

### Extract the ROI for each subject
for(subj in 1:length(subj.dat)){
  
  ### Obtain the file name from the subject folder
  file.name = dir(subj.dat[subj], pattern = "*", full.names = TRUE)[1]
  
  ### Load the dataset for a subject
  dat = readNIfTI(file.name)
  
  ### Extract the region
  ROI[ind1] = dat[ind]
  roi.list[[subj]] = ROI
  print(subj)
  
}

### Save the resulting ROI list
#setwd("/Users/ani/Dropbox/2021TDATexture/Rfiles")
##setwd("~aeloyan/ADNI")
###############################################################################
setwd("/Users/garyzhou/Downloads/Dropbox/garytestamygdala")
save(roi.list, file = "ROIlist_amygdala_full.rda")
save(subj.names, file = "SubjectNames_amygdala_full.rda")

#save(roi.list, file = "ROIlist_amygdala.rda")
#save(subj.names, file = "SubjectNames_amygdala.rda")
###############################################################################
setwd("/Users/garyzhou/Downloads/Dropbox/hippocampus")
save(roi.list, file = "ROIlist_hippocampus.rda")
save(subj.names, file = "SubjectNames_hippocampus.rda")
###############################################################################
setwd("/Users/garyzhou/Downloads/Dropbox/precentral")
save(roi.list, file = "ROIlist_precentral.rda")
save(subj.names, file = "SubjectNames_precentral.rda")
###############################################################################
setwd("/Users/garyzhou/Downloads/Dropbox/olfactory")
save(roi.list, file = "ROIlist_olfactory.rda")
save(subj.names, file = "SubjectNames_olfactory.rda")
