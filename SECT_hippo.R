library(BGLR)
library(doParallel)
library(Rcpp)
library(RcppArmadillo)
library(RcppParallel)

setwd("/Users/garyzhou/Downloads/Dropbox")
source("EC3D.R")

#Q1
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.04/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.04/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir, out.file = out.file, img.dir = NULL, first.only = FALSE)

#Q2
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.09/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.09/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q3
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.13/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.13/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q4
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.17/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.17/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q5
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.21/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.21/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q6
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.25/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.25/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q7
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.29/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.29/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q8
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.34/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.34/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)

#Q9
### Set up the Parameters ###
startdir = ""
in.dir = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.38/images"
out.file = "/Users/garyzhou/Downloads/Dropbox/hippo_tau7/Q1.38/MRIECs_hippo.RData"
img.dir = NULL
stepsize=100
rotstep=72
### Run The Euler Characteristic Function ###
ecf = ecf(in.dir = in.dir,out.file = out.file,img.dir = NULL,first.only = FALSE)
