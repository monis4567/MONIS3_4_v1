#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# 
#
# “MONIS4”
#
# Authors: Steen Wilhelm Knudsen.

#

# Change the working directory to a path on your own computer , and run
# the individual parts below to reproduce the diagrams presented in the paper
#
# All input data required needs to be available as csv-files in the same directory 
# as this R-code use for working directory.
#
# Occassionally the code will have difficulties producing the correct diagrams,
# if the packages and libraries are not installed.
# Make sure the packages are installed, and libraries are loaded, if the R-code
# fails in producing the diagrams.
#
#________________IMPORTANT!!_________________________________________________#
# (1)
#You have to change the path to the working directory before running this code
#
# (2)
# The 4 data input files required:
# "assay_no_to_spcnames_01.csv"
# "MONIS3_harbour_and_pos_water_samples03.csv"
# "outfile01_merged_txtrepfiles_from_mxpro.csv"
# "MONIS4_inv_spc_distr_tradit_survey_meth_2018may25_01.csv"
#
#
# must be located in the same working directory - as specified in the code below
#
#This code is able to run in:
#
#R studio: Version 0.98.994 – © 2009-2013 RStudio, Inc.
#R version:
# > version
# _                           
# platform       x86_64-pc-linux-gnu         
# arch           x86_64                      
# os             linux-gnu                   
# system         x86_64, linux-gnu           
# status                                     
# major          4                           
# minor          0.1                         
# year           2020                        
# month          06                          
# day            06                          
# svn rev        78648                       
# language       R                           
# version.string R version 4.0.1 (2020-06-06)
# nickname       See Things Now
#
#____________________________________________________________________________#

#remove everything in the working environment, without a warning!!
rm(list=ls())

#see this
#website
#on how to only install required packages
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scales, 
  fields, 
  gplots,
  plyr)#,
#ReporteRs)

## install the package 'scales', which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)

#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)

## install the package 'gplots', to be able to translate colors to hex - function: col2hex
#install.packages("gplots")
if(!require(gplots)){
  install.packages("gplots")
  library(gplots)
}
library(gplots)

## install the package 'glad', to be able to color using the function 'myPalette'
#install.packages("glad")
#library(glad)

require(graphics)

## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
#library(marmap)

#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
#library(TeachingDemos)

#get package to make maps
#install.packages("rworldmap")
#require (rworldmap)

#install.packages("rworldxtra")
#require(rworldxtra)

#get package to read excel files
#install.packages("readxl")
#library(readxl)

#get package to do count number of observations that have the same value at earlier records:
# see this website: https://stackoverflow.com/questions/11957205/how-can-i-derive-a-variable-in-r-showing-the-number-of-observations-that-have-th
#install.packages("plyr")
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
library(plyr)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("mapdata")
#library(mapdata)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
#library(maps)
# #get package for shapefiles see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
# install.packages(maptools)
# library(maptools)  #for shapefiles

# #get package for adding pies on the map
#install.packages("mapplots")
#library(mapplots)

#get the packages required for exporting to a table to word
#install.packages("ReporteRs")
# if(!require(ReporteRs)){
#   install.packages("ReporteRs")
#   library(ReporteRs)
# }

# devtools::install_github("davidgohel/ReporteRs")
# devtools::install_github("davidgohel/officer")

if(!require(officer)){
  install.packages("officer")
  library(officer)
}
#library(ReporteRs)
library(officer)


#install.packages("tableHTML")
#https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
if(!require(tableHTML)){
  install.packages("tableHTML")
  library(tableHTML)
}
require(tableHTML)

if(!require(envDocument)){
  install.packages("envDocument")
  library(envDocument)
}
require(envDocument)

wd00 <- "/home/hal9000/MONIS3_4_v1/"
# set working directory
#setwd("/Users/steenknudsen/Documents/Documents/Post doc KU/MONIS4/R_koder_for_eDNA_analyse_MONIS4")
#setwd ("/Users/steenknudsen/Documents/Documents/Post doc KU/MONIS3/")
wd10 <- "suppmatr10_plots_and_tables_from_Rcode"
wd09 <- "suppmatr09_Rcodes_for_analysing_qPCR_eDNA_data"
wd08 <- "suppmatr08_merged_qPCRdata_txt_reports"
#paste dirs together
wd <- paste0(wd00,wd09)
setwd(wd)
getwd()
#setwd('..')
#getwd()
#set(./)
#source()
#get_scriptpath()
#read excel with species names
#scpnmames <-as.data.frame(read_excel("DL_dk_specs_to_latspecs.xls"))
#names(scpnmames)

scpnmames <-as.data.frame(read.csv("suppmatr_09.01_assay_no_to_spcnames_03.csv",
                                   header = TRUE, sep = ",", quote = "\"",
                                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))

#read excel with harbours and positions
#harbours <-as.data.frame(read_excel("DL_harbour_and_pos_water_samples.xls"))

harbours <-as.data.frame(read.csv("suppmatr_09.02_MONIS3_harbour_and_pos_water_samples06.csv",
                                  header = TRUE, sep = ",", quote = "\"",
                                  dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))


#harbours
#harbours02 <- aggregate(harbours,
#                   by = list(harbours$Harbour),
#                   FUN = max)
#write.csv(harbours02, file = c(paste("MONIS4_harbours02.csv", sep="")))

# split text - see: https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
# and concatenate text - see: https://stackoverflow.com/questions/7201341/how-can-2-strings-be-concatenated 
# to get 6 letter abbr of latin speciesnames
ls.abbr.spcnm <-  paste(
  substr(sub('\\_.*', '', scpnmames$gen_specnm), 1, 3),
  substr(sub('.*\\_', '', scpnmames$gen_specnm), 1, 3),
  sep="."
)
#add back on to latin name dataframe
scpnmames$abbr.nm <- ls.abbr.spcnm

# set working directory
wd2 <- paste0(wd00,wd08)
setwd (wd2)
getwd()

#read csv with all merged mxpro results
smpls01 <- read.csv("outfile01_merged_txtrepfiles_from_mxpro.csv", header = TRUE, sep = ";", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
# set working directory
wd <- paste0(wd00,wd09)
setwd (wd)
getwd()

smpls02 <- smpls01

#smpls02
#smpls02 <- smpls02[smpls02=="Pseudochattonella serruculata"]<-"Pseudochattonella verruculosa "
#check foraar samples for Corcas
Corcas_foraar <- smpls02[ which(smpls02$speciesabbr=="Corcas" & smpls02$season=="foraar"), ]
Corcas_foraar$Harbour <- harbours$Harbour[match(Corcas_foraar$WellName, harbours$EuroFinsampleno)]


#check efteraar samples for Corcas
Corcas_efteraar <- smpls02[ which(smpls02$speciesabbr=="Corcas" & smpls02$season=="efteraar"), ]
Corcas_efteraar$Harbour <- harbours$Harbour[match(Corcas_efteraar$WellName, harbours$EuroFinsampleno)]


#remove blanks
#NOTE!! This will remove all NTC's with "No Ct"
smpls02<-na.omit(smpls02)
#remove "No Ct"
smpls02<-smpls02[!grepl("NoCt", smpls02$Quantitycopies),]

#change x into numeric variable
smpls02$CtdRn=as.numeric(as.character(smpls02$CtdRn))
smpls02$Quantitycopies=as.numeric(as.character(smpls02$Quantitycopies))

#amp$Volume<-as.character(amp$Volume)
#smpls02$WellName<-as.character(smpls02$WellType)
#names(amp)[names(amp)=="Volume"] <- "Reaction volume"
#names(smpls02)[names(smpls02)=="WellType"] <- "WellType"

#match between dataframes to add latin species names and DK common names
smpls02$gen_specnm <- scpnmames$gen_specnm[match(smpls02$speciesabbr, scpnmames$six_lett_spec_abbrv)]
smpls02$Genus <- scpnmames$Genus[match(smpls02$speciesabbr, scpnmames$six_lett_spec_abbrv)]
smpls02$species <- scpnmames$species[match(smpls02$speciesabbr, scpnmames$six_lett_spec_abbrv)]
smpls02$dk_comnm <- scpnmames$dk_comnm[match(smpls02$speciesabbr, scpnmames$six_lett_spec_abbrv)]

#paste a new column based on variables separated by point
harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno <- paste(harbours$EuroFinsampleno, harbours$Saeson, harbours$EuroFinsExtrSmpleno,  sep=".")
#paste a new column based on variables separated by point
smpls02$Wellname.season.no1 <- paste(smpls02$WellName, smpls02$season, "1",  sep=".")
#paste a new column based on variables separated by point
smpls02$gen_specnm.season <- paste(smpls02$gen_specnm, smpls02$season,  sep=".")
#paste a new column based on variables separated by point
smpls02$gen_specnm.season <- paste(smpls02$gen_specnm, smpls02$season,  sep=".")


#match between dataframes
smpls02$Harbour <- harbours$Harbour[match(smpls02$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls02$Coll_date <- harbours$Coll_date[match(smpls02$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls02$volfilt_mL <- harbours$volfilt_mL[match(smpls02$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls02$declat <- harbours$declat[match(smpls02$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls02$declon <- harbours$declon[match(smpls02$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
#paste a new column based on variables separated by point
smpls02$Harbour.Welltype <- paste(smpls02$Harbour, smpls02$WellType,  sep=".")
#get the unique smpl names for Harbours and WellTypes
unHaWT <- unique(smpls02$Harbour.Welltype)

# make a transparent color
transp_col <- rgb(0, 0, 0, 0)
#transp_col <- as.character("#FFFFFF")
HaWTnoNA <- addNA(unHaWT)
col.01<-as.numeric(as.factor(unHaWT))

#make a small dataframe w harbours and standards and numbers assigned, 
#use the col2hex in gplot pacakge to convert the 'red' color name to hex-color
col.02 <- col2hex(palette(rainbow(length(col.01))))
harbourcols <- cbind(unHaWT,col.01, col.02)

length(unHaWT)
length(col.01)
length(col.02)

#replace the colour for the standard dilution sample type with the transparent colour
col.03<-replace(col.02, col.01==15, transp_col)

col.04 <- cbind(harbourcols,col.03)
colforharb <- as.data.frame(col.04)

#match to main data frame and add as new color
smpls02$col.06 <- colforharb$col.03[match(smpls02$Harbour.Welltype, colforharb$unHaWT)]

#insert the transparent color for all matches with "NA.Standard"
smpls02$col.06[smpls02$Harbour.Welltype=="NA.Standard"] <- transp_col

#get the unique species names
latspecnm <- unique(smpls02$gen_specnm)

#match the assay number to the data frame with species
AIfps <- scpnmames$AssayIDNo[match(latspecnm, scpnmames$gen_specnm)]

#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
AIfps <-stringr::str_pad(AIfps, 2, pad = "0")

#make a new data frame with assay Id No and species
nlspnm <- data.frame(AIfps,latspecnm)

#reorder by the column 'AssayIDNo'
nlspnm<- nlspnm[order(nlspnm$AIfps),]

#make a list of numbers for the unique species
no.latspc <- seq(1:length(latspecnm))

#add a new column with no to use for appendix numbering
nlspnm <- cbind(nlspnm, no.latspc) 

#use the new order of latin species names for producing plots
latspecnm <- unique(nlspnm$latspecnm)

#latspecnm <- "Mnemiopsis_leidyi.foraar"
#latspecnm <- "Mnemiopsis_leidyi"
#latspecnm <- "Crassostrea_gigas.foraar"
#latspecnm <- "Crassostrea_gigas.efteraar"

#define the two seasons
dk.seasons <- c("foraar","efteraar")
eng.seasons <- c("spring","autumn")
tr.seasons <-  data.frame(dk.seasons, eng.seasons)

tr.seasons
class(tr.seasons)

amp <- smpls02






###############################################################################################
# start -calculate copies per L of filtered water
###############################################################################################


#remove blanks
#NOTE!! This will remove all NTC's with "No Ct"
#smpls02<-na.omit(smpls02)
#remove "No Ct"
#smpls02<-smpls02[!grepl("NoCt", smpls02$Quantitycopies),]

######
#copy the data frame
smpls02.1 <- smpls02
#set NA blanks to zero
smpls02.1$CtdRn[is.na(smpls02.1$CtdRn)] <- 0
smpls02.1$Quantitycopies[is.na(smpls02.1$Quantitycopies)] <- 0
#change x into numeric variable
#smpls02.1$CtdRn=as.numeric(as.character(smpls02.1$CtdRn))
#smpls02.1$Quantitycopies=as.numeric(as.character(smpls02.1$Quantitycopies))
#
colnames(smpls02.1)
#set NA blanks to zero
smpls02.1$CtdRn[is.na(smpls02.1$CtdRn)] <- 0
smpls02.1$Quantitycopies[is.na(smpls02.1$Quantitycopies)] <- 0

#make sure numbers are numbers
smpls02.1$Quantitycopies <- as.numeric(as.character(smpls02.1$Quantitycopies))
smpls02.1$volfilt_mL <- as.numeric(as.character(smpls02.1$volfilt_mL))

#set NA blanks to zero
smpls02.1$CtdRn[is.na(smpls02.1$CtdRn)] <- 0
smpls02.1$Quantitycopies[is.na(smpls02.1$Quantitycopies)] <- 0

#add column with copies per Liter of filtered water
#Ae = (Cqpcr /Fe) /Vwf. 
#’Ae’ number of  eDNA-copies per volumen filtered water, 
#’Cqpcr’ number of copies detected in the qPCR-well, #smpls02.1$meanQuantitycopies 
#’Fe’ the ratio of the eluted extrated filtrate used in a qPCR-well #5/350
#’Vwf’ is volumen of seawater filtered. #smpls02.1$volfilt_mL

#per mL
smpls02.1$copies_per_mLwater <- (smpls02.1$Quantitycopies/(5/350))/smpls02.1$volfilt_mL
#per Liter
smpls02.1$copies_per_Lwater <- smpls02.1$copies_per_mLwater*1000

#replace nas with zeros
smpls02.1$copies_per_Lwater[is.na(smpls02.1$copies_per_Lwater)]<-0
#add one to be able to do logarithmic scales
smpls02.1$copies_per_Lwater_plone<- smpls02.1$copies_per_Lwater+1

#take log10 to all copies
smpls02.1$log.10_copies_L <- log10(smpls02.1$copies_per_Lwater_plone)
#define path to where to write the file
wd00_10 <- paste0(wd00,wd10)
#write out a csv-file
write.csv(smpls02.1, file = paste0(wd00_10,"/","suppmatr_10.05_MONIS4_eDNA_smpls02.1.csv"))

###############################################################################################
# end -calculate copies per L of filtered water
###############################################################################################




################################################################################################
#   prepare a table of water samples
################################################################################################

#make a dataframe based on the unique names
harbnm <- as.data.frame(cbind(unique(harbours$Harbour),"foraar","efteraar"))
#alter the column names
colnames(harbnm) <- c("Harbour","season1","season2")

#subset this table by group
harbours.foraar <- harbours[ which(harbours$Saeson=="foraar"), ]
harbours.efteraar <- harbours[ which(harbours$Saeson=="efteraar"), ]

#add a column with the number for the extra Eurofins sample no
harbnm$season1.smpl1 <- 1
harbnm$season1.smpl2 <- 2
harbnm$season2.smpl1 <- 1
harbnm$season2.smpl2 <- 2

#add a new column that merges two columns
harbnm$Harbour.season1.smpl1 <- paste(as.character(harbnm$Harbour), harbnm$season1, harbnm$season1.smpl1, sep=".")
harbnm$Harbour.season1.smpl2 <- paste(as.character(harbnm$Harbour), harbnm$season1, harbnm$season1.smpl2, sep=".")
harbnm$Harbour.season2.smpl1 <- paste(as.character(harbnm$Harbour), harbnm$season2, harbnm$season1.smpl1, sep=".")
harbnm$Harbour.season2.smpl2 <- paste(as.character(harbnm$Harbour), harbnm$season2, harbnm$season1.smpl2, sep=".")
#make a new column with combined values to use for matching
harbours.foraar$Harbour.season.smpl <- paste(as.character(
  harbours.foraar$Harbour), harbours.foraar$Saeson, 
  harbours.foraar$EuroFinsExtrSmpleno, sep=".")
#make a new column with combined values to use for matching
harbours.efteraar$Harbour.season.smpl <- paste(as.character(
  harbours.efteraar$Harbour), harbours.efteraar$Saeson, 
  harbours.efteraar$EuroFinsExtrSmpleno, sep=".")
#match between data frames
harbnm$Coll_date.foraar.smpl1 <- harbours.foraar$Coll_date[match(harbnm$Harbour.season1.smpl1,harbours.foraar$Harbour.season.smpl)]
harbnm$Coll_date.foraar.smpl2 <- harbours.foraar$Coll_date[match(harbnm$Harbour.season1.smpl2,harbours.foraar$Harbour.season.smpl)]
harbnm$Coll_date.efteraar.smpl1 <- harbours.efteraar$Coll_date[match(harbnm$Harbour.season2.smpl1,harbours.efteraar$Harbour.season.smpl)]
harbnm$Coll_date.efteraar.smpl2 <- harbours.efteraar$Coll_date[match(harbnm$Harbour.season2.smpl2,harbours.efteraar$Harbour.season.smpl)]

#match between data frames
harbnm$volfilt_mL.foraar.smpl1 <- harbours.foraar$volfilt_mL[match(harbnm$Harbour.season1.smpl1,harbours.foraar$Harbour.season.smpl)]
harbnm$volfilt_mL.foraar.smpl2 <- harbours.foraar$volfilt_mL[match(harbnm$Harbour.season1.smpl2,harbours.foraar$Harbour.season.smpl)]
harbnm$volfilt_mL.efteraar.smpl1 <- harbours.efteraar$volfilt_mL[match(harbnm$Harbour.season2.smpl1,harbours.efteraar$Harbour.season.smpl)]
harbnm$volfilt_mL.efteraar.smpl2 <- harbours.efteraar$volfilt_mL[match(harbnm$Harbour.season2.smpl2,harbours.efteraar$Harbour.season.smpl)]

#keep only selected columns
keeps <- c("Harbour",
           "Coll_date.foraar.smpl1",
           "Coll_date.efteraar.smpl1",
           "Coll_date.foraar.smpl2",
           "Coll_date.efteraar.smpl2",
           "volfilt_mL.foraar.smpl1",
           "volfilt_mL.foraar.smpl2",
           "volfilt_mL.efteraar.smpl1",
           "volfilt_mL.efteraar.smpl2")
harbnm <- harbnm[keeps]

#reorder the columns in the dataframe, to make it easier to look at
harbnm <- harbnm[ c("Harbour",
                    "Coll_date.foraar.smpl1",
                    "volfilt_mL.foraar.smpl1",
                    "Coll_date.foraar.smpl2",
                    "volfilt_mL.foraar.smpl2",
                    "Coll_date.efteraar.smpl1",
                    "volfilt_mL.efteraar.smpl1",
                    "Coll_date.efteraar.smpl2",
                    "volfilt_mL.efteraar.smpl2"
) ]
#sort the data frame
harbnm2 <- harbnm[order(harbnm$Harbour),]
#define path to where to write the file
wd00_10 <- paste0(wd00,wd10)
#write to a csv file
write.csv(harbnm2, file = paste0(wd00_10,"/","suppmatr_10.05b_MONIS4_Table01_water_smpls_harbnm2.csv"))


################################################################################################
#   prepare a table of levels detected
################################################################################################

#get the LOD for each qPCR run
#use the function aggregate to get the minimum value for a group
lodtable1 <- aggregate(smpls02[, "Quantitycopies"], list(smpls02$gen_specnm.season, smpls02$WellType), min)
#subset this table by group
lodtable2 <- lodtable1[ which(lodtable1$Group.2=="Standard"), ]
#rename the column names
colnames(lodtable2) <- c("spc.season","WellT","LOD")

#identify LOQ for each qPCR run
#limit the dataframe to only well type that equals standard
oc<-smpls02[(smpls02$WellType=='Standard'),]
#add a new column that merges two columns
oc$Quan.spc.seas <- paste(oc$Quantitycopies, oc$gen_specnm.season,  sep=".")
#count the occurences of dilution steps - i.e. the number of succesful replicates
#see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
#and this webpage: https://stackoverflow.com/questions/9809166/count-number-of-rows-within-each-group
od<-oc %>% dplyr::count(Quantitycopies, gen_specnm.season)
#turn this into a dataframe
oe<-as.data.frame(od)
#add a new column that merges two columns
oe$Quan.spc.seas <- paste(as.character(oe$Quantitycopies), oe$gen_specnm.season,  sep=".")
#match the dilution step to the number of occurences -i.e. match between the two dataframes
no.occ <- oe$n[match(oc$Quan.spc.seas,oe$Quan.spc.seas)]
#add this column with counted occurences to the limited dataframe
og <- cbind.data.frame(oc,no.occ)
#exlude all observations where less than 3 replicates amplified
oh<-og[(og$no.occ>=3),]
#get the lowest dilution step that succesfully ampllified on all 3 repliactes
#use aggregate to get the minimum for each
loqtable1 <- aggregate(oh[, "Quantitycopies"], list(oh$gen_specnm.season), min)
#change the column names
colnames(loqtable1) <- c("spc.season","LOQ")

#copy the LOD table and add the corresponding LOQ values
loq.lod.table <- lodtable2
loq.lod.table$LOQ <- loqtable1$LOQ[match(lodtable2$spc.season,loqtable1$spc.season)]

#make a copy of the original data frame
smpls03 <- smpls01
#Replace all NoCts with zero
smpls03$Quantitycopies[smpls03$Quantitycopies=="NoCt"] <- 0
#delete rows that have "NoCtforFAMStandards" in the column 'smpls03$Quantitycopies'
#this error is caused by the wrong qpcr plate setup for  'Pseudochattonella_farcimen'
smpls03<-smpls03[!(smpls03$Quantitycopies=="NoCtforFAMStandards"),]

#check data frames for 'Pseudochattonella_farcimen'
#unique(smpls05$spc)
#unique(smpls01$speciesabbr)
#unique(smpls06$spc)
#psefar01<- smpls01[ which(smpls01$speciesabbr=='Psefar'),]
#psefar02<- smpls02[ which(smpls02$speciesabbr=='Psefar'),]
#psefar02$Quantitycopies
#psefar03<- smpls03[ which(smpls03$speciesabbr=='Psefar'),]
#Karmik03<- smpls03[ which(smpls03$speciesabbr=='Karmik'),]


#change x into numeric variable
smpls03$CtdRn=as.numeric(as.character(smpls03$CtdRn))
smpls03$Quantitycopies=as.numeric(as.character(smpls03$Quantitycopies))

#match between dataframes to add latin species names and DK common names
smpls03$gen_specnm <- scpnmames$gen_specnm[match(smpls03$speciesabbr, scpnmames$six_lett_spec_abbrv,nomatch="notfound")]
smpls03$Genus <- scpnmames$Genus[match(smpls03$speciesabbr, scpnmames$six_lett_spec_abbrv,nomatch="notfound")]
smpls03$species <- scpnmames$species[match(smpls03$speciesabbr, scpnmames$six_lett_spec_abbrv,nomatch="notfound")]
smpls03$dk_comnm <- scpnmames$dk_comnm[match(smpls03$speciesabbr, scpnmames$six_lett_spec_abbrv,nomatch="notfound")]

#paste a new column based on variables separated by point
harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno <- paste(harbours$EuroFinsampleno, harbours$Saeson, harbours$EuroFinsExtrSmpleno,  sep=".")
#paste a new column based on variables separated by point
smpls03$Wellname.season.no1 <- paste(smpls03$WellName, smpls03$season, "1",  sep=".")
#paste a new column based on variables separated by point
smpls03$gen_specnm.season <- paste(smpls03$gen_specnm, smpls03$season,  sep=".")
#paste a new column based on variables separated by point
smpls03$gen_specnm.season <- paste(smpls03$gen_specnm, smpls03$season,  sep=".")

#match between dataframes
smpls03$Harbour <- harbours$Harbour[match(smpls03$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls03$Coll_date <- harbours$Coll_date[match(smpls03$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls03$volfilt_mL <- harbours$volfilt_mL[match(smpls03$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls03$declat <- harbours$declat[match(smpls03$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
smpls03$declon <- harbours$declon[match(smpls03$Wellname.season.no1, harbours$EuroFinsampleno.Saeson.EuroFinsExtrSmpleno)]
#paste a new column based on variables separated by point
smpls03$Harbour.Welltype <- paste(smpls03$Harbour, smpls03$WellType,  sep=".")
#get the unique smpl names for Harbours and WellTypes
unHaWT <- unique(smpls03$Harbour.Welltype)

#transp_col <- as.character("#FFFFFF")
HaWTnoNA <- addNA(unHaWT)
col.01<-as.numeric(as.factor(HaWTnoNA))

#make a small dataframe w harbours and standards and numbers assigned, 
#use the col2hex in gplot pacakge to convert the 'red' color name to hex-color
col.02 <- col2hex(palette(rainbow(length(col.01)+1)))
harbourcols <- cbind(unHaWT,col.01, col.02)

length(unHaWT)
length(col.01) 
length(col.02)

#replace the colour for the standard dilution sample type with the transparent colour
col.03<-replace(col.02, col.01==15, transp_col)

col.04 <- cbind(harbourcols,col.03)
colforharb <- as.data.frame(col.04)

#match to main data frame and add as new color
smpls03$col.06 <- colforharb$col.03[match(smpls03$Harbour.Welltype, colforharb$unHaWT)]

#match between the smpls03 dataframe and the LOD and LOQ table
smpls03$LOD <- loq.lod.table$LOD[match(smpls03$gen_specnm.season, loq.lod.table$spc.season)]
smpls03$LOQ <- loq.lod.table$LOQ[match(smpls03$gen_specnm.season, loq.lod.table$spc.season)]

#make a subset 
smpls04 <- smpls03[ which(smpls03$WellType=="Unknown"), ]

######################################################################################################
# Get mean for each set of 3 technical qPCR replicates per species per season per harbour
######################################################################################################
#get the mean quantity for each species per season per port
smpls05 <- aggregate(smpls04[, "Quantitycopies"], list(smpls04$gen_specnm.season, smpls04$Harbour), mean)
#change the column names
colnames(smpls05) <- c("spc.season","Harbour","meanQuantitycopies")

#match with LOD and LOQ
smpls05$LOD <- loq.lod.table$LOD[match(smpls05$spc.season, loq.lod.table$spc.season)]
smpls05$LOQ <- loq.lod.table$LOQ[match(smpls05$spc.season, loq.lod.table$spc.season)]

#add an empty column with just NAs
smpls05[,"eDNA_eval_mean"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls05$eDNA_eval_mean[smpls05$meanQuantitycopies>smpls05$LOQ] <- "aboveLOQ"
smpls05$eDNA_eval_mean[smpls05$meanQuantitycopies<smpls05$LOQ] <- "AbLOD_BeLOQ"
smpls05$eDNA_eval_mean[smpls05$meanQuantitycopies<smpls05$LOD & !smpls05$meanQuantitycopies==0] <- "belowLOD"
smpls05$eDNA_eval_mean[smpls05$meanQuantitycopies==0] <- "NoCt"

#add an empty column with just NAs to fil with color codings
smpls05[,"eDNA_col_eval_mean"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls05$eDNA_col_eval_mean[smpls05$meanQuantitycopies>smpls05$LOQ] <- "black" # "green"
smpls05$eDNA_col_eval_mean[smpls05$meanQuantitycopies<smpls05$LOQ] <- "red" #"orange" 
smpls05$eDNA_col_eval_mean[smpls05$meanQuantitycopies<smpls05$LOD & !smpls05$meanQuantitycopies==0] <-"yellow" #"yellow"
smpls05$eDNA_col_eval_mean[smpls05$meanQuantitycopies==0] <- "white" #,"red"
#reorder the dataframe, to make it easier to look at
smpls06 <- smpls05[ order(smpls05$spc.season, smpls05$Harbour), ]

#use the library stringr to use the function 'str_split_fixed', to split a column and bind it back to the original 
library(stringr)
spec_season_df <- cbind(as.data.frame(str_split_fixed(smpls06$spc.season, "\\.", 2)), smpls06$spc.season)
#change the column names
colnames(spec_season_df) <- c("spc","season","spc.season")

#match back with the original data frame
smpls06$spc <- spec_season_df$spc[match(smpls06$spc.season, spec_season_df$spc.season)]
smpls06$season <- spec_season_df$season[match(smpls06$spc.season, spec_season_df$spc.season)]


#write to a csv file
#write out a csv-file
write.csv(smpls06, file = paste0(wd00_10,"/","suppmatr_10.05e_MONIS4_eDNA_smpls06.csv"))


#make subsets 
smpls07foraar <- smpls06[ which(smpls06$season=="foraar"), ]
smpls07efteraar <- smpls06[ which(smpls06$season=="efteraar"), ]

#make copies of the dataset
smpls08foraar <- smpls07foraar
smpls08efteraar <-smpls07efteraar

#keep only selected columns
keeps <- c("spc", "Harbour", "eDNA_eval_mean")
smpls08foraar <- smpls08foraar[keeps]
smpls08efteraar <- smpls08efteraar[keeps]

#rearrange the data fram using the reshape function
nn<-reshape(smpls08foraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls08foraar$Harbour)
nn[is.na(nn)]<-0
smpls09foraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls08efteraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls08efteraar$Harbour)
nn[is.na(nn)]<-0
smpls09efteraar <- nn
#define path to where to write the file
wd00_10 <- paste0(wd00,wd10)
#write to a csv file
write.csv(smpls09foraar, file = paste0(wd00_10,"/","suppmatr_10.05b_smpls09foraar_eDNA_eval_on_avr.csv"))
write.csv(smpls09efteraar, file = paste0(wd00_10,"/","suppmatr_10.05b_smpls09efteraar_eDNA_eval_on_avr.csv"))


######################################################################################################
# prepare table with eDNA evalution for higest detection lvl for each of 
# 3 techincal qPCR replicates per species per season per harbour
######################################################################################################

#copy the data frame
smpls10 <- smpls04

#check harbours in foraar for Corcas
#Corcas_foraar <- smpls04[ which(smpls04$gen_specnm.season=="Cordylophora_caspia.foraar"), ]
#unique(Corcas_foraar$Harbour)

#match with LOD and LOQ
smpls10$LOD <- loq.lod.table$LOD[match(smpls10$gen_specnm.season, loq.lod.table$spc.season)]
smpls10$LOQ <- loq.lod.table$LOQ[match(smpls10$gen_specnm.season, loq.lod.table$spc.season)]

#add an empty column with just NAs to fil with color codings
smpls10[,"eDNA_eval_mean"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls10$eDNA_eval_mean[smpls10$Quantitycopies>smpls10$LOQ] <- "aboveLOQ" #3 #"green" 
smpls10$eDNA_eval_mean[smpls10$Quantitycopies<smpls10$LOQ] <-  "AbLOD_BeLOQ" #2 #"orange"
smpls10$eDNA_eval_mean[smpls10$Quantitycopies<smpls10$LOD & !smpls10$Quantitycopies==0] <- "belowLOD" #1 #"yellow" 
smpls10$eDNA_eval_mean[smpls10$Quantitycopies==0] <- "NoCt" #1 #"red" 

#count the number of evaluations per species per harbour per season for each set of 3 replicates
pc<-smpls10 #copy the data frame
#add a new column with unified values from columns
pc$gen_specnm.Wellname.season.no1.eDNA_eval_mean <- paste(pc$gen_specnm, pc$Wellname.season.no1, pc$eDNA_eval_mean, sep=".")
#count the occurences of dilution steps - i.e. the number of succesful replicates
#see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
#and this webpage: https://stackoverflow.com/questions/9809166/count-number-of-rows-within-each-group
pd <- dplyr::count(pc, gen_specnm.Wellname.season.no1.eDNA_eval_mean)
#turn this into a dataframe
pe<-as.data.frame(pd)
#match the dilution step to the number of occurences -i.e. match between the two dataframes
no.occ <- pe$n[match(pc$gen_specnm.Wellname.season.no1.eDNA_eval_mean,pe$gen_specnm.Wellname.season.no1.eDNA_eval_mean)]
#add this column with counted occurences to the limited dataframe
pg <- cbind.data.frame(pc,no.occ)

#add another column
pg$gen_specnm.Wellname.season.no1 <- paste(pg$gen_specnm, pg$Wellname.season.no1, sep=".")

#make subsets
pg_aboveLOQ <- pg[ which(pg$eDNA_eval_mean=="aboveLOQ"), ]
pg_AbLOD_BeLOQ <- pg[ which(pg$eDNA_eval_mean=="AbLOD_BeLOQ"), ]
pg_belowLOD <- pg[ which(pg$eDNA_eval_mean=="belowLOD"), ]
pg_NoCt <- pg[ which(pg$eDNA_eval_mean=="NoCt"), ]

#count for each of the subsets
ph_aboveLOQ <- dplyr::count(pg_aboveLOQ, gen_specnm.Wellname.season.no1)
ph_AbLOD_BeLOQ<- dplyr::count(pg_AbLOD_BeLOQ, gen_specnm.Wellname.season.no1)
ph_belowLOD<- dplyr::count(pg_belowLOD, gen_specnm.Wellname.season.no1)
ph_NoCt<- dplyr::count(pg_NoCt, gen_specnm.Wellname.season.no1)
#turn into data frames
pi_aboveLOQ<-as.data.frame(ph_aboveLOQ)
pi_AbLOD_BeLOQ<-as.data.frame(ph_AbLOD_BeLOQ)
pi_belowLOD<-as.data.frame(ph_belowLOD)
pi_NoCt<-as.data.frame(ph_NoCt)

#match between dataframes to add latin species names and DK common names
smpls06$Genus <- scpnmames$Genus[match(smpls06$spc, scpnmames$gen_specnm)]
smpls06$species <- scpnmames$species[match(smpls06$spc, scpnmames$gen_specnm)]
smpls06$dk_comnm <- scpnmames$dk_comnm[match(smpls06$spc, scpnmames$gen_specnm)]

#paste a new column based on variables separated by point
harbours$Harbour.Saeson.EuroFinsExtrSmpleno <- paste(harbours$Harbour, harbours$Saeson, harbours$EuroFinsExtrSmpleno,  sep=".")
smpls06$Harbour.Saeson.EuroFinsExtrSmpleno <- paste(smpls06$Harbour, smpls06$season, "1",  sep=".")

#match between dataframes
smpls06$Harbour <- harbours$Harbour[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]
smpls06$Coll_date <- harbours$Coll_date[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]
smpls06$volfilt_mL <- harbours$volfilt_mL[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]
smpls06$declat <- harbours$declat[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]
smpls06$declon <- harbours$declon[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]
smpls06$EuroFinsampleno <- harbours$EuroFinsampleno[match(smpls06$Harbour.Saeson.EuroFinsExtrSmpleno, harbours$Harbour.Saeson.EuroFinsExtrSmpleno)]

#paste a new column based on variables separated by point
smpls06$gen_specnm.Wellname.season.no1 <- paste(smpls06$spc, smpls06$EuroFinsampleno, smpls06$season, "1", sep=".")

#match counts back with data frame
smpls06$freq_NoCt <- pi_NoCt$n[match(smpls06$gen_specnm.Wellname.season.no1, pi_NoCt$gen_specnm.Wellname.season.no1)]
smpls06$freq_belowLOD <- pi_belowLOD$n[match(smpls06$gen_specnm.Wellname.season.no1, pi_belowLOD$gen_specnm.Wellname.season.no1)]
smpls06$freq_AbLOD_BeLOQ <- pi_AbLOD_BeLOQ$n[match(smpls06$gen_specnm.Wellname.season.no1, pi_AbLOD_BeLOQ$gen_specnm.Wellname.season.no1)]
smpls06$freq_aboveLOQ <- pi_aboveLOQ$n[match(smpls06$gen_specnm.Wellname.season.no1, pi_aboveLOQ$gen_specnm.Wellname.season.no1)]

#replace NAs with zeros
smpls06$freq_NoCt[is.na(smpls06$freq_NoCt)] <- 0
smpls06$freq_belowLOD[is.na(smpls06$freq_belowLOD)] <- 0
smpls06$freq_AbLOD_BeLOQ[is.na(smpls06$freq_AbLOD_BeLOQ)] <- 0
smpls06$freq_aboveLOQ[is.na(smpls06$freq_aboveLOQ)] <- 0

#make a new column that shows the replicates quality levels
smpls06$freq_repl_eval <- as.character(paste("'",smpls06$freq_NoCt,"/", smpls06$freq_belowLOD,"/", smpls06$freq_AbLOD_BeLOQ,"/", smpls06$freq_aboveLOQ,"'", sep=""))

#add an empty column with just NAs to fil with color codings
smpls06[,"eDNA_eval_p_repl_col"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls06$eDNA_eval_p_repl_col[smpls06$freq_NoCt>=0] <- 1 #"white" #"NoCt" #0  
smpls06$eDNA_eval_p_repl_col[smpls06$freq_belowLOD>=1] <- 2 #"yellow" #"belowLOD" #1  
smpls06$eDNA_eval_p_repl_col[smpls06$freq_AbLOD_BeLOQ>=1] <- 3 # "azure3" #"AbLOD_BeLOQ" #2 
smpls06$eDNA_eval_p_repl_col[smpls06$freq_aboveLOQ>=1] <- 4 #"azure4" #"one aboveLOQ" #3 
smpls06$eDNA_eval_p_repl_col[smpls06$freq_aboveLOQ>=3] <- 5 #"black" #"all 3 above LOQ" #4

#add an empty column with just NAs to fil with evaluations
smpls06[,"eDNA_eval_p_repl_descr"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls06$eDNA_eval_p_repl_descr[smpls06$freq_NoCt>=0] <- "NoCt" #0  
smpls06$eDNA_eval_p_repl_descr[smpls06$freq_belowLOD>=1] <- "belowLOD" #1  
smpls06$eDNA_eval_p_repl_descr[smpls06$freq_AbLOD_BeLOQ>=1] <- "AbLOD_BeLOQ" #2 
smpls06$eDNA_eval_p_repl_descr[smpls06$freq_aboveLOQ>=1] <- "1aboveLOQ" #3 
smpls06$eDNA_eval_p_repl_descr[smpls06$freq_aboveLOQ>=3] <- "3aboveLOQ" #4 

#match AssID No back with data frame
smpls06$AssayIDcode <- scpnmames$AssayIDcode[match(smpls06$spc, scpnmames$gen_specnm)]

#use the sub function to replace, and put this in a new column
smpls06$AssayIDNo <-  paste(sub('AssID', '', smpls06$AssayIDcode))

#use the sub function to replace, and put this in a new column
scpnmames$AssayIDNo <-  paste(sub('AssID', '', scpnmames$AssayIDcode))


#make subsets 
smpls11foraar <- smpls06[ which(smpls06$season=="foraar"), ]
smpls11efteraar <- smpls06[ which(smpls06$season=="efteraar"), ]

#make copies of the dataset
smpls12foraar <- smpls11foraar
smpls12efteraar <-smpls11efteraar

#keep only selected columns for freqlevels
keeps <- c("spc", "Harbour", "freq_repl_eval")
smpls13foraar <- smpls12foraar[keeps]
smpls13efteraar <- smpls12efteraar[keeps]
#keep only selected columns for coloring
keeps <- c("spc", "Harbour", "eDNA_eval_p_repl_col")
smpls14foraar <- smpls12foraar[keeps]
smpls14efteraar <- smpls12efteraar[keeps]


#rearrange the data fram using the reshape function
nn<-reshape(smpls13foraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls13foraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls13foraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls13efteraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls13efteraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls13efteraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls14foraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls14foraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls14foraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls14efteraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls14efteraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls14efteraar <- nn




##########################################################################################
#
# Make a summary table of 'spring' and 'fall' eDNA-samples
#
##########################################################################################

#merge the two dataframes
smpls15 <- merge(smpls14foraar,smpls14efteraar,by="spc", suffixes = c(".for", ".eft"))

#get the highest value between two columns
#https://stackoverflow.com/questions/28531809/i-want-to-select-the-greater-of-the-two-values-from-two-columns-in-r
smpls15$AalborgHavn.max.cat <- pmax(smpls15$AalborgHavn.for, smpls15$AalborgHavn.eft)
smpls15$Aalborgportland.max.cat <- pmax(smpls15$Aalborgportland.for, smpls15$Aalborgportland.eft)
smpls15$Aarhus.max.cat <- pmax(smpls15$Aarhus.for, smpls15$Aarhus.eft)
smpls15$Esbjerg.max.cat <- pmax(smpls15$Esbjerg.for, smpls15$Esbjerg.eft)
smpls15$Fredericia.max.cat <- pmax(smpls15$Fredericia.for, smpls15$Fredericia.eft)
smpls15$Frederikshavn.max.cat <- pmax(smpls15$Frederikshavn.for, smpls15$Frederikshavn.eft)
smpls15$Gedser.max.cat <- pmax(smpls15$Gedser.for, smpls15$Gedser.eft)
smpls15$Grenaa.max.cat <- pmax(smpls15$Grenaa.for, smpls15$Grenaa.eft)
smpls15$Helsingoer.max.cat <- pmax(smpls15$Helsingoer.for, smpls15$Helsingoer.eft)
smpls15$Hirtshals.max.cat <- pmax(smpls15$Hirtshals.for, smpls15$Hirtshals.eft)
smpls15$Kalundborg.max.cat <- pmax(smpls15$Kalundborg.for, smpls15$Kalundborg.eft)
smpls15$KalundborgStatiolHavn.max.cat <- pmax(smpls15$KalundborgStatiolHavn.for, smpls15$KalundborgStatiolHavn.eft)
smpls15$Koebenhavn.max.cat <- pmax(smpls15$Koebenhavn.for, smpls15$Koebenhavn.eft)
smpls15$Koege.max.cat <- pmax(smpls15$Koege.for, smpls15$Koege.eft)
smpls15$Odense.max.cat <- pmax(smpls15$Odense.for, smpls15$Odense.eft)
smpls15$Roedby.max.cat <- pmax(smpls15$Roedby.for, smpls15$Roedby.eft)


#start by subsetting the dataframes:
#subset by the max.cat  columns
# see this wbesite: http://r.789695.n4.nabble.com/How-to-extract-data-frame-columns-using-regex-td904878.html
keep.col01 <- colnames(smpls15[,grepl('max.cat',colnames(smpls15)) ])
keep.col02 <- colnames(smpls15[,grepl('Assay',colnames(smpls15)) ])
#keep.col03 <- colnames(smpls15[,grepl('spc',colnames(smpls15)) ])
keeps <- c(keep.col01, keep.col02)
smpls16 <- smpls15[keeps]
#add a column with species name
smpls16$spc <- smpls15$spc[match(smpls16$AssayIDNo.for,smpls15$AssayIDNo.for)]
#sort the dataframe by a specific column
smpls17 <- smpls16[order(smpls16$AssayIDNo.for),] 
#colnames(smpls17)
#reorder the columns
ncol00.smp17 <- ncol(smpls17)
ncol01.smp17 <- ncol(smpls17)-1
ncol02.smp17 <- ncol(smpls17)-2
ncol03.smp17 <- ncol(smpls17)-3

smpls18 <- smpls17[,c(ncol01.smp17,ncol00.smp17,1:ncol03.smp17)]

#using the categories scored earlier, simplify these scores even further 
# to represent
# 0, 1 and 2
# 1 #"white" #"NoCt" #0  = new value: 0
# 2 #"yellow" #"belowLOD" #1 = new value: 1  
# 3 # "azure3" #"AbLOD_BeLOQ" #2 = new value: 1 
# 4 #"azure4" #"one aboveLOQ" #3 = new value: 2
# 5 #"black" #"all 3 above LOQ" #4 = new value: 2

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=0] <- 0
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=0] <- 0
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=0] <- 0
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=0] <- 0
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=0] <- 0
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=0] <- 0
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=0] <- 0
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=0] <- 0
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=0] <- 0
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=0] <- 0
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=0] <- 0
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=0] <- 0
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=0] <- 0
smpls18$Koege02eval[smpls18$Koege.max.cat>=0] <- 0
smpls18$Odense02eval[smpls18$Odense.max.cat>=0] <- 0
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=0] <- 0

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=1] <- 0
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=1] <- 0
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=1] <- 0
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=1] <- 0
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=1] <- 0
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=1] <- 0
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=1] <- 0
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=1] <- 0
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=1] <- 0
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=1] <- 0
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=1] <- 0
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=1] <- 0
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=1] <- 0
smpls18$Koege02eval[smpls18$Koege.max.cat>=1] <- 0
smpls18$Odense02eval[smpls18$Odense.max.cat>=1] <- 0
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=1] <- 0

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=2] <- 1
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=2] <- 1
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=2] <- 1
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=2] <- 1
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=2] <- 1
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=2] <- 1
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=2] <- 1
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=2] <- 1
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=2] <- 1
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=2] <- 1
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=2] <- 1
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=2] <- 1
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=2] <- 1
smpls18$Koege02eval[smpls18$Koege.max.cat>=2] <- 1
smpls18$Odense02eval[smpls18$Odense.max.cat>=2] <- 1
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=2] <- 1

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=3] <- 1
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=3] <- 1
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=3] <- 1
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=3] <- 1
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=3] <- 1
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=3] <- 1
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=3] <- 1
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=3] <- 1
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=3] <- 1
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=3] <- 1
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=3] <- 1
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=3] <- 1
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=3] <- 1
smpls18$Koege02eval[smpls18$Koege.max.cat>=3] <- 1
smpls18$Odense02eval[smpls18$Odense.max.cat>=3] <- 1
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=3] <- 1

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=4] <- 2
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=4] <- 2
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=4] <- 2
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=4] <- 2
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=4] <- 2
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=4] <- 2
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=4] <- 2
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=4] <- 2
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=4] <- 2
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=4] <- 2
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=4] <- 2
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=4] <- 2
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=4] <- 2
smpls18$Koege02eval[smpls18$Koege.max.cat>=4] <- 2
smpls18$Odense02eval[smpls18$Odense.max.cat>=4] <- 2
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=4] <- 2

#asssign new category values
smpls18$AalborgHavn02eval[smpls18$AalborgHavn.max.cat>=5] <- 2
smpls18$Aalborgportland02eval[smpls18$Aalborgportland.max.cat>=5] <- 2
smpls18$Aarhus02eval[smpls18$Aarhus.max.cat>=5] <- 2
smpls18$Esbjerg02eval[smpls18$Esbjerg.max.cat>=5] <- 2
smpls18$Fredericia02eval[smpls18$Fredericia.max.cat>=5] <- 2
smpls18$Frederikshavn02eval[smpls18$Frederikshavn.max.cat>=5] <- 2
smpls18$Gedser02eval[smpls18$Gedser.max.cat>=5] <- 2
smpls18$Grenaa02eval[smpls18$Grenaa.max.cat>=5] <- 2
smpls18$Helsingoer02eval[smpls18$Helsingoer.max.cat>=5] <- 2
smpls18$Hirtshals02eval[smpls18$Hirtshals.max.cat>=5] <- 2
smpls18$Kalundborg02eval[smpls18$Kalundborg.max.cat>=5] <- 2
smpls18$KalundborgStatiolHavn02eval[smpls18$KalundborgStatiolHavn.max.cat>=5] <- 2
smpls18$Koebenhavn02eval[smpls18$Koebenhavn.max.cat>=5] <- 2
smpls18$Koege02eval[smpls18$Koege.max.cat>=5] <- 2
smpls18$Odense02eval[smpls18$Odense.max.cat>=5] <- 2
smpls18$Roedby02eval[smpls18$Roedby.max.cat>=5] <- 2


#subset dataframe again
keep.col01 <- colnames(smpls18[,grepl('02eval',colnames(smpls18)) ])
keeps <- c(keep.col01, "spc", "AssayIDNo.eft")
smpls19 <- smpls18[keeps]
#sort the dataframe by a specific column
smpls20 <- smpls19[order(smpls19$AssayIDNo.eft),] 
colnames(smpls20)
#reorder the columns
ncol00.smp20 <- ncol(smpls20)
ncol01.smp20 <- ncol(smpls20)-1
ncol02.smp20 <- ncol(smpls20)-2
ncol03.smp20 <- ncol(smpls20)-3
#rearrange columns
smpls21 <- smpls20[,c(ncol00.smp20,ncol01.smp20,1:ncol02.smp20)]
#transpose the dataframe
#https://stackoverflow.com/questions/6778908/transpose-a-data-frame

library(data.table)
# transpose
t_smpls21 <- transpose(smpls21)
# get row and colnames in order
colnames(t_smpls21) <- rownames(smpls21)
rownames(t_smpls21) <- colnames(smpls21)

colnames(t_smpls21) <- t_smpls21[1,]

#replace in rownames
rownames(t_smpls21) <- gsub("02eval","", rownames(t_smpls21))

#t_smpls21
#define path to where to write the file
wd00_10 <- paste0(wd00,wd10)
#write the table to a csv
write.csv(t_smpls21, file = paste0(wd00_10,"/","suppmatr_10.05b_MONIS4_eDNA_summary_table6.1.csv"))





#from this website:
#http://www.sthda.com/english/wiki/add-a-table-into-a-word-document-using-r-software-and-reporters-package

#https://davidgohel.github.io/ReporteRs/articles/FlexTable.html


#get the packages required for exporting to a table to word
#install.packages("ReporteRs")
#library(ReporteRs)

#################################################################################
#  prepare tables for efteraar
#################################################################################
# set working directory
setwd(wd)
getwd()
#make a dataframe without the first two columns
ee <- smpls14efteraar[,3:ncol(smpls14efteraar)]

####################################################################################
# Start Appendix C
####################################################################################

##########################################################################################
#
# Make a table that looks somewhat similar to Table 5 presented by :
#  Li, J, Hatton‐Ellis, TW, Lawson Handley, L‐J, et al. Ground‐truthing of a fish‐based environmental DNA metabarcoding method for assessing the quality of lakes. J Appl Ecol. 2019; 56: 1232– 1244. https://doi.org/10.1111/1365-2664.13352 
#
# Make a table for each year sampled
##########################################################################################
#copy the data frame
# head(smpls06,6)
# 
smpls22 <- smpls06

#count the number of season to loop over
nooseas <- length(unique(smpls22$season))
# make a sequence of numbers to use in a data frame
no_se <- seq(1:nooseas)
#smpls22$season2 <- gsub("efteraar","fall",smpls22$season)
#smpls22$season2 <- gsub("foraar","fall",smpls22$season)
#get the names of the seasons -  to use in the loop below
cas <- sort(unique(smpls22$season))
ssp <- c("spring","foraar")
sfa <- c("fall","efteraar")
# make names for the seasons
nms.sea <- c("spring","fall")
# bind to a data frame
se_nms <- as.data.frame(t(cbind(ssp,sfa)))
colnames(se_nms) <- c("eng","dan")
se_nms <- cbind(se_nms, no_se)
# make one of the columns numeric
se_nms$no_se <- as.numeric(se_nms$no_se)


#define the columns to keep 
keeps <- c("EuroFinsampleno",
           "spc",
           "eDNA_eval_p_repl_descr",
           "Harbour.Saeson.EuroFinsExtrSmpleno",
           "season",
           "Coll_date")
#keep only selected columns
smpls23 <- smpls22[keeps]
#count number of rows
#nrow(smpls23)
#keep unique rows only
smpls24_df <- smpls23
#smpls24_df <- smpls23 %>% dplyr::distinct(spc, EuroFinsampleno, .keep_all = TRUE)
# Sort by vector name [smpltp] then [gen_specnm.year_inds] # https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/

smpls24_df <- smpls24_df[
  with(smpls24_df, order(EuroFinsampleno, season)),
  ]
# Sort by vector name [smpltp] then [gen_specnm.year_inds] # https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/

#use strsplit to get second element
smpls24_df$month <- data.frame(do.call('rbind', strsplit(as.character(smpls24_df$Coll_date),'.',fixed=TRUE)))[,2]
#use strsplit to get first element
smpls24_df$year <-as.numeric(data.frame(do.call('rbind', strsplit(as.character(smpls24_df$Coll_date),'.',fixed=TRUE)))[,1])
# make a new column that fuses MST sample number together with month
#MONIS5eDNA12_df$smpltp.month <- paste(MONIS5eDNA12_df$smpltp,".",MONIS5eDNA12_df$month_inds2,sep="")
smpls24_df$EuroFinsampleno.year.month <- paste(smpls24_df$EuroFinsampleno,".",smpls24_df$year,".",smpls24_df$month,sep="")
smpls24_df$EuroFinsampleno.year <- paste(smpls24_df$EuroFinsampleno,".",smpls24_df$year,sep="")
smpls24_df$spc.EuroFinsampleno.year <- paste(smpls24_df$spc,".",smpls24_df$EuroFinsampleno,".",smpls24_df$year,sep="")
smpls24_df$spc.season <- paste(smpls24_df$spc,".",smpls24_df$season,sep="")
#https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame
library(dplyr)
smpls24_2_df <- smpls24_df %>% group_by(smpls24_df$spc.EuroFinsampleno.year) %>% mutate(id = row_number())
smpls24_3_df <- as.data.frame(smpls24_2_df)
names(smpls24_3_df)[names(smpls24_3_df) == 'smpls24_df$spc.EuroFinsampleno.year'] <- 'spc.EuroFinsampleno.year2'
smpls24_3_df$spc.id <- paste(smpls24_3_df$spc,".",smpls24_3_df$id,sep="")
#smpls24_3_df$gen_specnm <- gsub("\\..*","",smpls24_3_df$gen_specnm.season)
#smpls24_3_df$spc.id <- paste(smpls24_3_df$gen_specnm,".",smpls24_3_df$id,sep="")
smpls24_df <- smpls24_3_df
#as.data.frame(table(smpls24_df$gen_specnm.WellName.year))
#head(smpls24_df, 4)
#split column by delimiter, and turn in to data frame # https://www.rdocumentation.org/packages/splitstackshape/versions/1.4.8/topics/cSplit
smpls25_df <- as.data.frame(splitstackshape::cSplit(smpls24_df,"spc.season", sep = "."))
#Rename specific column # see :  https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
names(smpls25_df)[names(smpls25_df) == 'gen_specnm.season_2'] <- 'season2'
names(smpls25_df)[names(smpls25_df) == 'gen_specnm.season_1'] <- 'gen_specnm'

#head(smpls25_df,12)
#use the year listed in a vector previously
yrs <- unique(smpls25_df$year)
# use just a single year to start with for testing the loop
#seasons <- "efteraar"
#yr_smpl <- "2017"
categories.of.seasons <- unique(smpls25_df$season)
#categories.of.seasons <- "foraar"
#loop over years sampled -  to produce individual tables per year sampled
for (yr_smpl in yrs){
    print(yr_smpl)
  #}
  #subset based on variable values - only retain rows where the column that match the criteria 
  #sbs.MO13y_df <- MONIS5eDNA13_df[ which(MONIS5eDNA13_df$yrs_smpl==yr_smpl), ]
  sbs.s26_df <- smpls25_df[ which(smpls25_df$year==yr_smpl), ]
  #to try out the loop assign only one category
  #categories.of.seasons <- "season_1"
  #loop over the seasons
  for (season in categories.of.seasons){
    print(season)
    #}
    #subset based on variable values - only retain rows where the column that match the criteria 
    sbs.s27_df <- sbs.s26_df[ which(sbs.s26_df$season==season), ]
    #head(sbs.s27_df,4)
    #define the columns to keep 
    # keeps <- c("gen_specnm",
    #            "eDNA_eval_p_repl_descr",
    #            "smpltp.month")
    keeps <- c("eDNA_eval_p_repl_descr",
               "Harbour.Saeson.EuroFinsExtrSmpleno",
               #"EuroFinsampleno",
               "spc")
    #keep only selected columns
    #colnames(sbs.s27_df)
    sbs.s28_df <- sbs.s27_df[keeps]
    # see which row have an 'NA' value
    which(is.na(sbs.s28_df$eDNA_eval_p_repl_descr))
    #sbs.s28_df[rowSums(is.na(sbs.s28_df))==0,]
    #substitute in one column
    sbs.s28_df$Harbour <- gsub("\\..*","",sbs.s28_df$Harbour.Saeson.EuroFinsExtrSmpleno)
    #delete column
    sbs.s28_df$Harbour.Saeson.EuroFinsExtrSmpleno <- NULL
    #reshape the data frame to have smpls for columns
    sbs.s29_df <- reshape(sbs.s28_df, idvar = "spc", timevar = "Harbour", direction = "wide")
    #Replace characters in column names gsub : # https://stackoverflow.com/questions/39670918/replace-characters-in-column-names-gsub
    #head(sbs.s29_df, 3)
    names(sbs.s29_df) <- gsub(x = names(sbs.s29_df), pattern = "eDNA_eval_p_repl_descr\\.", replacement = "")  
    #count the number of columns
    nc.s29 <- ncol(sbs.s29_df)
    
    #use match to match the season with a data frame and get the name for the season
    spcfc_seaon_name <- se_nms$eng[match(season, se_nms$dan)]
    spcfc_seaon_name <- as.character(spcfc_seaon_name)
    #use match to match the season with a data frame and get the category number for the season
    spcfc_seaon_no <- se_nms$no_se[match(season, se_nms$dan)]
    spcfc_seaon_no <- as.numeric(spcfc_seaon_no)
    
    if (dim(sbs.s29_df)[1] == 0) {
      print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
      sbs.s29_df <- as.data.frame(rbind(c("MST_smpl01","MST_smpl02"),c("no_data","sampled")))
    }
    
    #replace values in entire data frame
    sbs.s29_df[sbs.s29_df=="NoCt"]<-"NoCq"
    sbs.s29_df[sbs.s29_df=="belowLOD"]<-"bLOD" 
    sbs.s29_df[sbs.s29_df=="AbLOD_BeLOQ"]<-"aLODbLOQ"
    sbs.s29_df[sbs.s29_df=="1aboveLOQ"]<-"1aLOQ"
    sbs.s29_df[sbs.s29_df=="3aboveLOQ"]<-"3aLOQ"
    #get the unique years sampled
    yrs <- yr_smpl
    # Remove columns from dataframe where ALL values are NA # https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
    sbs.s29_df <- sbs.s29_df[,colSums(is.na(sbs.s29_df))<nrow(sbs.s29_df)]
    # delete the first column from the data frame
    #sbs.MO_df[,1] <- NULL
    
    sbs.MO_df <- sbs.s29_df
    #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
    if(!require(tableHTML)){
      install.packages("tableHTML")
      library(tableHTML)
    }
    require(tableHTML)
    #try the tableHTML with no border
    tableHTML <- sbs.MO_df %>% 
      tableHTML(border = 0) 
    #count the number of columns in the dataframe
    l.s.MO <- length(sbs.MO_df)
    #get unique cell values in dataframe : see : http://r.789695.n4.nabble.com/Retrieve-distinct-values-within-a-whole-data-frame-td1460205.html
    #apart from the first column
    unique(unlist(sbs.MO_df[2:l.s.MO]))
    #make lists of the words in the cells to color using the 'add_css_conditional_column' function
    eDNA.lvl01 <- c("NoCq") #white
    eDNA.lvl02 <- c("bLOD") #yellow
    eDNA.lvl03 <- c("aLODbLOQ") #orange
    eDNA.lvl04 <- c("1aLOQ") #red
    eDNA.lvl05 <- c("3aLOQ") #black
    #place the data frame in a tableHTML object
    tableHTML <- sbs.MO_df %>% 
      tableHTML()
    # for eDNA.lvl01 <- c("NoCq") #white
    words <- eDNA.lvl01 #<- c("NoCq") #white
    col.f.cell <- "white"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl02 <- c("beLOD") #yellow
    words <- eDNA.lvl02
    col.f.cell <- "yellow"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl03 <- c("abLODbeLOQ") #orange
    words <- eDNA.lvl03
    col.f.cell <- "orange"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl04 <- c("1abLOQ") #red
    words <- eDNA.lvl04
    col.f.cell <- "red"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl05 <- c("3abLOQ") #black
    words <- eDNA.lvl05
    col.f.cell <- "black" # use this color for the cell
    col.f.font <- "white" #use this color for the font
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color","color"),
                                              c(col.f.cell,col.f.font)))
    }
    
    t.HTML17 <- tableHTML
    
    
    #pad with zeros to two characters
    #see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
    no.e4 <-stringr::str_pad(spcfc_seaon_no, 2, pad = "0")
    #head(t.HTML17)
    #and to export in a file
    
    write_tableHTML(t.HTML17, file = paste(wd00_10,"/","suppmatr_10.05b_App_C_",yr_smpl,"_",no.e4,"_",spcfc_seaon_name,"_table_eDNA_evalu.html", sep=""))
    
    #end loop over seasons
  }
  #end loop over years
}

####################################################################################
# End Appendix C
####################################################################################












####################################################################################
# Start Appendix D
####################################################################################

##########################################################################################
#
# Make a table that looks somewhat similar to Table 5 presented by :
#  Li, J, Hatton‐Ellis, TW, Lawson Handley, L‐J, et al. Ground‐truthing of a fish‐based environmental DNA metabarcoding method for assessing the quality of lakes. J Appl Ecol. 2019; 56: 1232– 1244. https://doi.org/10.1111/1365-2664.13352 
#
# Make a table for each year sampled
##########################################################################################
#copy the data frame
# head(smpls06,6)
# 
smpls22 <- smpls06

#count the number of season to loop over
nooseas <- length(unique(smpls22$season))
# make a sequence of numbers to use in a data frame
no_se <- seq(1:nooseas)
#smpls22$season2 <- gsub("efteraar","fall",smpls22$season)
#smpls22$season2 <- gsub("foraar","fall",smpls22$season)
#get the names of the seasons -  to use in the loop below
cas <- sort(unique(smpls22$season))
ssp <- c("spring","foraar")
sfa <- c("fall","efteraar")
# make names for the seasons
nms.sea <- c("spring","fall")
# bind to a data frame
se_nms <- as.data.frame(t(cbind(ssp,sfa)))
colnames(se_nms) <- c("eng","dan")
se_nms <- cbind(se_nms, no_se)
# make one of the columns numeric
se_nms$no_se <- as.numeric(se_nms$no_se)


#define the columns to keep 
keeps <- c("EuroFinsampleno",
           "spc",
           "freq_repl_eval",
           "Harbour.Saeson.EuroFinsExtrSmpleno",
           "season",
           "Coll_date")
#keep only selected columns
smpls23 <- smpls22[keeps]

#
#count number of rows
#nrow(smpls23)
#keep unique rows only
smpls24_df <- smpls23
#smpls24_df <- smpls23 %>% dplyr::distinct(spc, EuroFinsampleno, .keep_all = TRUE)
# Sort by vector name [smpltp] then [gen_specnm.year_inds] # https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/

smpls24_df <- smpls24_df[
  with(smpls24_df, order(EuroFinsampleno, season)),
  ]
# Sort by vector name [smpltp] then [gen_specnm.year_inds] # https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/

#use strsplit to get second element
smpls24_df$month <- data.frame(do.call('rbind', strsplit(as.character(smpls24_df$Coll_date),'.',fixed=TRUE)))[,2]
#use strsplit to get first element
smpls24_df$year <-as.numeric(data.frame(do.call('rbind', strsplit(as.character(smpls24_df$Coll_date),'.',fixed=TRUE)))[,1])
# make a new column that fuses MST sample number together with month
#MONIS5eDNA12_df$smpltp.month <- paste(MONIS5eDNA12_df$smpltp,".",MONIS5eDNA12_df$month_inds2,sep="")
smpls24_df$EuroFinsampleno.year.month <- paste(smpls24_df$EuroFinsampleno,".",smpls24_df$year,".",smpls24_df$month,sep="")
smpls24_df$EuroFinsampleno.year <- paste(smpls24_df$EuroFinsampleno,".",smpls24_df$year,sep="")
smpls24_df$spc.EuroFinsampleno.year <- paste(smpls24_df$spc,".",smpls24_df$EuroFinsampleno,".",smpls24_df$year,sep="")
smpls24_df$spc.season <- paste(smpls24_df$spc,".",smpls24_df$season,sep="")
#https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame
library(dplyr)
smpls24_2_df <- smpls24_df %>% group_by(smpls24_df$spc.EuroFinsampleno.year) %>% mutate(id = row_number())
smpls24_3_df <- as.data.frame(smpls24_2_df)
names(smpls24_3_df)[names(smpls24_3_df) == 'smpls24_df$spc.EuroFinsampleno.year'] <- 'spc.EuroFinsampleno.year2'
smpls24_3_df$spc.id <- paste(smpls24_3_df$spc,".",smpls24_3_df$id,sep="")
#smpls24_3_df$gen_specnm <- gsub("\\..*","",smpls24_3_df$gen_specnm.season)
#smpls24_3_df$spc.id <- paste(smpls24_3_df$gen_specnm,".",smpls24_3_df$id,sep="")
smpls24_df <- smpls24_3_df
#as.data.frame(table(smpls24_df$gen_specnm.WellName.year))
#head(smpls24_df, 4)
#split column by delimiter, and turn in to data frame # https://www.rdocumentation.org/packages/splitstackshape/versions/1.4.8/topics/cSplit
smpls25_df <- as.data.frame(splitstackshape::cSplit(smpls24_df,"spc.season", sep = "."))
#Rename specific column # see :  https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
names(smpls25_df)[names(smpls25_df) == 'gen_specnm.season_2'] <- 'season2'
names(smpls25_df)[names(smpls25_df) == 'gen_specnm.season_1'] <- 'gen_specnm'

#head(smpls25_df,12)
#use the year listed in a vector previously
yrs <- unique(smpls25_df$year)
# use just a single year to start with for testing the loop
#seasons <- "efteraar"
#yr_smpl <- "2017"
categories.of.seasons <- unique(smpls25_df$season)
#categories.of.seasons <- "foraar"
#loop over years sampled -  to produce individual tables per year sampled
for (yr_smpl in yrs){
  print(yr_smpl)
  #}
  #subset based on variable values - only retain rows where the column that match the criteria 
  #sbs.MO13y_df <- MONIS5eDNA13_df[ which(MONIS5eDNA13_df$yrs_smpl==yr_smpl), ]
  sbs.s26_df <- smpls25_df[ which(smpls25_df$year==yr_smpl), ]
  #to try out the loop assign only one category
  #categories.of.seasons <- "season_1"
  #loop over the seasons
  for (season in categories.of.seasons){
    print(season)
    #}
    #subset based on variable values - only retain rows where the column that match the criteria 
    sbs.s27_df <- sbs.s26_df[ which(sbs.s26_df$season==season), ]
    #head(sbs.s27_df,4)
    #define the columns to keep 
    # keeps <- c("gen_specnm",
    #            "eDNA_eval_p_repl_descr",
    #            "smpltp.month")
    keeps <- c("freq_repl_eval",
               "Harbour.Saeson.EuroFinsExtrSmpleno",
               #"EuroFinsampleno",
               "spc")
    #keep only selected columns
    #colnames(sbs.s27_df)
    sbs.s28_df <- sbs.s27_df[keeps]
    # see which row have an 'NA' value
    which(is.na(sbs.s28_df$eDNA_eval_p_repl_descr))
    #sbs.s28_df[rowSums(is.na(sbs.s28_df))==0,]
    #substitute in one column
    sbs.s28_df$Harbour <- gsub("\\..*","",sbs.s28_df$Harbour.Saeson.EuroFinsExtrSmpleno)
    #delete column
    sbs.s28_df$Harbour.Saeson.EuroFinsExtrSmpleno <- NULL
    #reshape the data frame to have smpls for columns
    sbs.s29_df <- reshape(sbs.s28_df, idvar = "spc", timevar = "Harbour", direction = "wide")
    #Replace characters in column names gsub : # https://stackoverflow.com/questions/39670918/replace-characters-in-column-names-gsub
    #head(sbs.s29_df, 3)
    names(sbs.s29_df) <- gsub(x = names(sbs.s29_df), pattern = "freq_repl_eval\\.", replacement = "")  
    #count the number of columns
    nc.s29 <- ncol(sbs.s29_df)
    
    #use match to match the season with a data frame and get the name for the season
    spcfc_seaon_name <- se_nms$eng[match(season, se_nms$dan)]
    spcfc_seaon_name <- as.character(spcfc_seaon_name)
    #use match to match the season with a data frame and get the category number for the season
    spcfc_seaon_no <- se_nms$no_se[match(season, se_nms$dan)]
    spcfc_seaon_no <- as.numeric(spcfc_seaon_no)
    
    if (dim(sbs.s29_df)[1] == 0) {
      print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
      sbs.s29_df <- as.data.frame(rbind(c("MST_smpl01","MST_smpl02"),c("no_data","sampled")))
    }
    
    #get the unique years sampled
    yrs <- yr_smpl
    # Remove columns from dataframe where ALL values are NA # https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
    sbs.s29_df <- sbs.s29_df[,colSums(is.na(sbs.s29_df))<nrow(sbs.s29_df)]
    # delete the first column from the data frame
    #sbs.MO_df[,1] <- NULL
    
    #sbs.s29_df
    #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
    if(!require(tableHTML)){
      install.packages("tableHTML")
      library(tableHTML)
    }
    require(tableHTML)
    
    sbs.36_df <- sbs.s29_df
    #get the number of columns
    nc.s36 <- ncol(sbs.36_df)
    # get unique values in the data frame from a selected range of columns # see : https://stackoverflow.com/questions/40003028/extracting-unique-values-from-data-frame-using-r
    unqval.s36 <- unique(unlist((sbs.36_df)[,2:nc.s36]))
    #remove NA from vector - this will exclde the non-evaluated categories
    unqval.s36 <- unqval.s36[!is.na(unqval.s36)]
    
    # With the grep function in R the different elements in 'freq_repl_eval' 
    # can be categorized for 
    # identification later on in the preparation of the html table that is to show
    # the colored categories for eDNA levels detected.
    # The coding for the elements in 'freq_repl_eval' are:
    # ' no of replicates with no Ct/no of replicates below LOD /no of replicates above LOD but below LOQ / no of replicates above LOQ ' 
    # the color coding for these elements in 'freq_repl_eval' are
    # ' white/yellow /orange / red or black '
    # red if a minimum if 1 replicate is above LOQ (disregarding if any lower levels are detected)
    # black if all replicates are above LOQ (this will automatically equal all lower categories being zero)
    # To try out the grep function, I have here below tried grepping for different elements in the list
    #grep for elements that begin with '0
    grep("^'0", unqval.s36, value=T)
    #grep for elements that end with 0'
    grep("0'$", unqval.s36, value=T)
    # grep for elements starting '0/0/0/ and then 1 or any higher number
    b_cat <- grep("'0/0/0/[1-9]+", unqval.s36, value=T) # this will equal the black category with all replicates amplyfiying
    # then grep for all elements with not zero in the last category
    br_cat <- grep("'[0-9]+/[0-9]+/[0-9]+/[1-9]+", unqval.s36, value=T) # this will equal both the red and balck category
    # find the difference between these two vectors - i.e. subtract the black category from the fused red-black category
    r_cat <- setdiff(br_cat,b_cat)
    # grep for all elements with not zero in the first category
    w_cat <- grep("'[1-9]+/0/0/0'", unqval.s36, value=T) # this will equal all replicates not amplifying - i.e. this will equal the white category
    # find the difference between two vectors
    wyo_cat <- setdiff(unqval.s36,br_cat)
    # find the difference between two vectors
    yo_cat <- setdiff(wyo_cat,w_cat)
    # grep for all elements with not zero in the last and second last category
    y_cat <- grep("'[0-9]+/[1-9]+/0/0'", unqval.s36, value=T)
    # grep for all elements with not zero in thelast category
    o_cat <- grep("'[0-9]+/[0-9]+/[1-9]+/0'", unqval.s36, value=T)
    #these categories are used below to identify the color coding in the html tables
    
    sbs.MO_df <- sbs.36_df
    #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
    if(!require(tableHTML)){
      install.packages("tableHTML")
      library(tableHTML)
    }
    require(tableHTML)
    #try the tableHTML with no border
    tableHTML <- sbs.MO_df %>% 
      tableHTML(border = 0) 
    #count the number of columns in the dataframe
    l.s.MO <- length(sbs.MO_df)
    #get unique cell values in dataframe : see : http://r.789695.n4.nabble.com/Retrieve-distinct-values-within-a-whole-data-frame-td1460205.html
    #apart from the first column
    unique(unlist(sbs.MO_df[2:l.s.MO]))
    #make lists of the words in the cells to color using the 'add_css_conditional_column' function
    class(eDNA.lvl01)
    eDNA.lvl01 <- w_cat #c("/0/0/0'") #white
    eDNA.lvl02 <- y_cat #c("'0/") #yellow
    eDNA.lvl03 <- o_cat #c("aLODbLOQ") #orange
    eDNA.lvl04 <- r_cat #c("/1'") #red
    eDNA.lvl05 <- b_cat #c("'0/0/0/") #black
    #place the data frame in a tableHTML object
    tableHTML <- sbs.MO_df %>% 
      tableHTML()
    # for eDNA.lvl01 <- c("NoCq") #white
    words <- eDNA.lvl01 #<- c("NoCq") #white
    col.f.cell <- "white"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl02 <- c("beLOD") #yellow
    words <- eDNA.lvl02
    col.f.cell <- "yellow"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl03 <- c("abLODbeLOQ") #orange
    words <- eDNA.lvl03
    col.f.cell <- "orange"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl04 <- c("1abLOQ") #red
    words <- eDNA.lvl04
    col.f.cell <- "red"
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color"),
                                              c(col.f.cell)))
    }
    # for eDNA.lvl05 <- c("3abLOQ") #black
    words <- eDNA.lvl05
    col.f.cell <- "black" # use this color for the cell
    col.f.font <- "white" #use this color for the font
    for (word in words) {
      tableHTML <- tableHTML %>% 
        add_css_conditional_column(columns = 2:l.s.MO, #make it work on column 2 to the last column
                                   conditional = "contains",
                                   value = word,
                                   css = list(c("background-color","color"),
                                              c(col.f.cell,col.f.font)))
    }
    
    t.HTML17 <- tableHTML
    #pad with zeros to two characters
    #see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
    no.e4 <-stringr::str_pad(spcfc_seaon_no, 2, pad = "0")
    #head(t.HTML17)
    #and to export in a file
    write_tableHTML(t.HTML17, file = paste(wd00_10,"/suppmatr_10.05b_App_D_",yr_smpl,"_",no.e4,"_",spcfc_seaon_name,"_table_eDNA_evalu.html", sep=""))
    
    #end loop over seasons
  }
  #end loop over years
}
####################################################################################
# End Appendix D
####################################################################################









#http://labrtorian.com/2016/11/07/conditional-formatting-of-a-table-in-r/
#http://www.sthda.com/english/wiki/add-a-table-into-a-word-document-using-r-software-and-reporters-package

####################################################################################
# Import data with records of conventional monitoring of invasive species
# and compare this data frame with records from eDNA monitoring
####################################################################################

# set working directory
#setwd(wd)
#paste paths together
wd00_09 <- paste(wd00,wd09,sep = "")
setwd(wd00_09)
#import a data frame with species recorded by conventional survey
srtsurv01 <-as.data.frame(read.csv("suppmatr_09.03_MONIS4_inv_spc_distr_tradit_survey_meth_2018jun14_04.csv",
                                   header = TRUE, sep = ",", quote = "\"",
                                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))

#replace the wrong spc name with the correct spc name
srtsurv01$Species_name[srtsurv01$Species_name == "Pseudochattonella_serruculata"] <- "Pseudochattonella_verruculosa"
srtsurv01$Species_name
srtsurv02 <- srtsurv01
#prefix to all column names
colnames(srtsurv02) <- paste("convrec", colnames(srtsurv02), sep = ".")
#reshape from wide to long # see this example: https://stackoverflow.com/questions/2185252/reshaping-data-frame-from-wide-to-long-format
#srtsurv02<- reshape(srtsurv01, direction = "long", varying = list(names(srtsurv01)[3:ncol(srtsurv01)]), 
#                    v.names = "conv_rec_val", idvar = c("Species_name","spcNo"), 
#                    timevar = "Harbour", times = names(srtsurv01)[3:ncol(srtsurv01)])

srtsurv02<- reshape(srtsurv01, direction = "long", varying = list(names(srtsurv01)[3:ncol(srtsurv01)]), 
                    v.names = "conv_rec_val", idvar = c("Species_name"), 
                    timevar = "Harbour", times = names(srtsurv01)[3:ncol(srtsurv01)])
#paste a new column based on variables separated by point
srtsurv02$Species_name.Harbour <- paste(srtsurv02$Species_name, srtsurv02$Harbour,  sep=".")
smpls06$spc.Harbour <- paste(smpls06$spc, smpls06$Harbour,  sep=".")

#match counts in "trad_rec_val" back with data frame with eDNA samples
smpls06$conv_rec_val <- srtsurv02$conv_rec_val[match(smpls06$spc.Harbour, srtsurv02$Species_name.Harbour)]

#replace NA with zero
#smpls06$conv_rec_val[is.na(smpls06$conv_rec_val)]<-0
#add a column to color by conventional monitoring
#add an empty column with just NAs to fil with evaluations
smpls06[,"col.f.conv_rec_val"] <- NA
#replace in the empty column, the order is important, 
#as you otherwise will end up with the last evaluations
smpls06$col.f.conv_rec_val[smpls06$conv_rec_val==0 ] <- "white"# "red" #not recorded before, very unlikely
smpls06$col.f.conv_rec_val[smpls06$conv_rec_val==1 ] <- "red" #"azure3" #"yellow" # recorded before MONIS4
smpls06$col.f.conv_rec_val[smpls06$conv_rec_val==2 ] <- "black" # "green" # recorded during MONIS4
#validated combined score values from both eDNA and conventional
#valid_comb_score
vsc01 <- c("score_eDNA","eDNA_RESULT","description","score_conv","abbr_conv_descr","valid_comb_score","valid_hexcol","valid_score")
vsc02 <- c("1","NoCt","Not_found_ever_","0","NF","1_and_0","#92D050","1")
vsc03 <- c("2","BeLOD","Not_found_ever_","0","NF","2_and_0","#C4D79B","2")
vsc04 <- c("3","AbLOD","Not_found_ever_","0","NF","3_and_0","#C4D79B","3")
vsc05 <- c("4","1AbLOQ","Not_found_ever_","0","NF","4_and_0","#DA9694","4")
vsc06 <- c("5","3AbLOQ","Not_found_ever_","0","NF","5_and_0","#FF0000","5")
vsc07 <- c("1","NoCt","Found_before_but_not_during_Monis4_field_work","1","FB","1_and_1","#C4D79B","6")
vsc08 <- c("2","BeLOD","Found_before_but_not_during_Monis4_field_work","1","FB","2_and_1","#C4D79B","7")
vsc09 <- c("3","AbLOD","Found_before_but_not_during_Monis4_field_work","1","FB","3_and_1","#C4D79B","8")
vsc10 <- c("4","1AbLOQ","Found_before_but_not_during_Monis4_field_work","1","FB","4_and_1","#C4D79B","9")
vsc11 <- c("5","3AbLOQ","Found_before_but_not_during_Monis4_field_work","1","FB","5_and_1","#C4D79B","10")
vsc12 <- c("1","NoCt","Found_during_Monis_4_field_work","2","FM4","1_and_2","#538DD5","11")
vsc13 <- c("2","BeLOD","Found_during_Monis_4_field_work","2","FM4","2_and_2","#8DB4E2","12")
vsc14 <- c("3","AbLOD","Found_during_Monis_4_field_work","2","FM4","3_and_2","#92D050","13")
vsc15 <- c("4","1AbLOQ","Found_during_Monis_4_field_work","2","FM4","4_and_2","#92D050","14")
vsc16 <- c("5","3AbLOQ","Found_during_Monis_4_field_work","2","FM4","5_and_2","#92D050","15")

#bind columns and transpose
vsc <- t(cbind(vsc01,vsc02,vsc03,vsc04,vsc05,
               vsc06,vsc07,vsc08,vsc09,vsc10,
               vsc11,vsc12,vsc13, vsc14, vsc15, vsc16))

#assign col names based on first row
colnames(vsc) = vsc[1, ]
#remove the first row
vsc = vsc[-1, ] 

#turn into data frame
vsc <- as.data.frame(vsc)

#paste columns to make new columns
smpls06$valid_comb_score <- paste(smpls06$eDNA_eval_p_repl_col ,"_and_" ,smpls06$conv_rec_val ,sep="")
vsc$categ_valid <- paste(vsc$eDNA_RESULT,"_" ,vsc$abbr_conv_descr ,sep="")

#match to data frame
smpls06$valid_hexcol <- vsc$valid_hexcol[match(smpls06$valid_comb_score, vsc$valid_comb_score)]
smpls06$valid_score <- as.numeric(vsc$valid_score[match(smpls06$valid_comb_score, vsc$valid_comb_score)])
smpls06$categ_valid <- vsc$categ_valid[match(smpls06$valid_comb_score, vsc$valid_comb_score)]


#make subsets 
smpls15foraar <- smpls06[ which(smpls06$season=="foraar"), ]
smpls15efteraar <- smpls06[ which(smpls06$season=="efteraar"), ]

#make copies of the dataset
smpls16foraar <- smpls15foraar
smpls16efteraar <-smpls15efteraar

#keep only selected columns for freqlevels
keeps <- c("spc", "Harbour", "categ_valid")
smpls17foraar <- smpls16foraar[keeps]
smpls17efteraar <- smpls16efteraar[keeps]
#keep only selected columns for coloring
keeps <- c("spc", "Harbour", "valid_score")
smpls18foraar <- smpls16foraar[keeps]
smpls18efteraar <- smpls16efteraar[keeps]



#rearrange the data fram using the reshape function
nn<-reshape(smpls17foraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls17foraar$Harbour)

nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls17foraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls17efteraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls17efteraar$Harbour)

nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls17efteraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls18foraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls18foraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls18foraar <- nn

#rearrange the data fram using the reshape function
nn<-reshape(smpls18efteraar,timevar="Harbour",idvar="spc",direction="wide")
names(nn)[-1]<-as.character(smpls18efteraar$Harbour)
nn[is.na(nn)]<-0
#match the assay Id No
nn$AssayIDNo <- scpnmames$AssayIDNo[match(nn$spc, scpnmames$gen_specnm)]
#reorder columns, to make Ass ID No appear earlier in the data frame
nn <- nn[c("spc", "AssayIDNo", "AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "KalundborgStatiolHavn", "Koebenhavn", "Koege", "Odense", "Roedby")]
#reorder rows by assay ID No
nn <- nn[order(nn$AssayIDNo),]
#refer to the data frame by a new name
smpls18efteraar <- nn

# make additional columns
smpls17foraar$spc.season <- paste(smpls17foraar$spc,".spring",sep="")
smpls17foraar$season <- "spring"
smpls17foraar$seasonno <- 1
smpls17efteraar$spc.season <- paste(smpls17efteraar$spc,".fall",sep="")
smpls17efteraar$season <- "fall"
smpls17efteraar$seasonno <- 2
# bind dataframes together
s17s_df <- dplyr::bind_rows(smpls17foraar, smpls17efteraar)
#get unique seasons
unqseas <- unique(s17s_df$season)

#https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
if(!require(tableHTML)){
  install.packages("tableHTML")
  library(tableHTML)
}
require(tableHTML)


# iterate over data frames
for (seas_nm in unqseas){ 
  #print(seas_nm)
#}
  
  #subset based on variable values - only retain rows where the column that match the criteria 
  sbs.MO_df <- s17s_df[ which(s17s_df$season==seas_nm), ]
  seas_no <- unique(sbs.MO_df$seasonno)
  #count number of columns
  nc.sbs.MO1_df <- ncol(sbs.MO_df)
  nc.sbs.MO2_df <- (nc.sbs.MO1_df-2)
  #delete column from data frame : see: https://stackoverflow.com/questions/6286313/remove-an-entire-column-from-a-data-frame-in-r
  sbs.MO2_df <- sbs.MO_df[,-(nc.sbs.MO2_df:nc.sbs.MO1_df)]
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#try the tableHTML with no border
tableHTML <- sbs.MO2_df %>% 
  tableHTML(border = 0) 
#count the number of columns in the dataframe
l.s.MO <- length(sbs.MO2_df)
#get unique cell values in dataframe : see : http://r.789695.n4.nabble.com/Retrieve-distinct-values-within-a-whole-data-frame-td1460205.html
#apart from the first two columns
#unique(unlist(sbs.MO_df[3:l.s.MO]))
#make lists of the words in the cells to color using the 'add_css_conditional_column' function
#make evaluation categories for whether
# eDNA method matches conventional  
# by following these categories:
# NoCt_FM4	blue	not recorded with eDNA, but found in the present study by conventional monitoring
# AbLOD_FM4	light blue	above LOD with eDNA, and found in the present study by conventional monitoring
# BeLOD_FB	light blue	below LOD with eDNA, but found before conventionally
# BeLOD_FM4	light blue	below LOD with eDNA, and found in the present study by conventional monitoring
# NoCt_FB	light blue	not recorded with eDNA, but found before conventionally
# 1AbLOQ_FB	light red	1 repl above LOQ with eDNA, and found before conventionally
# 3AbLOQ_FB	light red	3 repl above LOQ with eDNA, and found before conventionally
# AbLOD_FB	light red	above LOD with eDNA, and found before conventionally
# AbLOD_NF	light red	above LOD with eDNA, and never found before conventionally
# BeLOD_NF	light red	below LOD with eDNA, and never found before conventionally
# 1AbLOQ_NF	red	1 repl above LOQ with eDNA, but never found before conventionally
# 3AbLOQ_NF	red	3 repl above LOQ with eDNA, but never found before conventionally
# 1AbLOQ_FM4	white	1 repl above LOQ with eDNA, and found in the present study by conventional monitoring
# 3AbLOQ_FM4	white	3 repl above LOQ with eDNA, and found in the present study by conventional monitoring
# NoCt_NF	white	not recorded with eDNA, and never found before conventionally

#methods  disagree, eDNA says no, traditional monitoring says yes
eval.cat01 <- c("NoCt_FM4") # blue
#methods almost disagree, eDNA says no, traditional monitoring says yes
eval.cat02 <- c("AbLOD_FM4", "BeLOD_FB", "BeLOD_FM4", "NoCt_FB") # light blue
#methods almost disagree, eDNA says yes, traditional monitoring says no
eval.cat03 <- c("1AbLOQ_FB", "3AbLOQ_FB", "AbLOD_FB", "AbLOD_NF", "BeLOD_NF") # light red
#methods disagree, eDNA says yes, traditional monitoring says no
eval.cat04 <- c("1AbLOQ_NF", "3AbLOQ_NF") # red
#methods agree
eval.cat05 <- c("1AbLOQ_FM4", "3AbLOQ_FM4", "NoCt_NF") # white

#place the data frame in a tableHTML object
tableHTML <- sbs.MO2_df %>% 
  tableHTML()
# assign the eval category to a variable
words <- eval.cat01 #  blue category
col.f.cell <- "blue" # use this color for the cell
col.f.font <- "white" #use this color for the font
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 3:l.s.MO, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color","color"),
                                          c(col.f.cell,col.f.font)))
}
# assign the eval category to a variable
words <- eval.cat02 # light blue category
# color the cells if it is a match
col.f.cell <- "cornflowerblue"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 3:l.s.MO, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}

# assign the eval category to a variable
words <- eval.cat03 # light red category
# color the cells if it is a match
col.f.cell <- "#eea2ad" #light red
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 3:l.s.MO, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# assign the eval category to a variable
words <- eval.cat04 #red
# color the cells if it is a match
col.f.cell <- "#ee3a54" #darker red
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 3:l.s.MO, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# assign the eval category to a variable
words <- eval.cat05 #whte
# color the cells if it is a match
col.f.cell <- "white" #white category
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 3:l.s.MO, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}

t.HTML17 <- tableHTML
#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
no.e4 <-stringr::str_pad(seas_no, 2, pad = "0")

write_tableHTML(t.HTML17, file = paste(wd00_10,"/suppmatr_10.05c_App_D_",yr_smpl,"_",no.e4,"_",seas_nm,"_table_eDNA_evalu_and_conv_eval.html", sep=""))
#end iterating over seasons in data frame
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#################################################################################
#  prepare tables for  concentration in copies per Liter of seawater
#################################################################################
#copy the data frame
smpls20 <- smpls06
#add column with copies per Liter of filtered water
#Ae = (Cqpcr /Fe) /Vwf. 
#’Ae’ number of  eDNA-copies per volumen filtered water, 
#’Cqpcr’ number of copies detected in the qPCR-well, #smpls20$meanQuantitycopies 
#’Fe’ the ratio of the eluted extrated filtrate used in a qPCR-well #5/350
#’Vwf’ is volumen of seawater filtered. #smpls20$volfilt_mL
#per mL
smpls20$copies_per_mLwater <- (smpls20$meanQuantitycopies/(5/350))/smpls20$volfilt_mL
#per Liter
smpls20$copies_per_Lwater <- smpls20$copies_per_mLwater*1000
#replace nas with zeros
smpls20$copies_per_Lwater[is.na(smpls20$copies_per_Lwater)]<-0
#add one to be able to do logarithmic scales
smpls20$copies_per_Lwater_plone<- smpls20$copies_per_Lwater+1
#take log10 to all copies
smpls20$log.10_copies_L <- log10(smpls20$copies_per_Lwater_plone)
#add an empty column with just NAs to fil with evaluations
smpls20[,"no_for_log.10_eDNAlvls"] <- NA
#replace in the empty column, the order is important, 
#as you otherwise will end up with the last evaluations
smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L==0 ] <- 1 #if No Ct

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L<=
                                 log10((smpls20$LOD/(5/350))/smpls20$volfilt_mL*1000) 
                               & smpls20$log.10_copies_L>0] <- 2 #if below LOD but above zero

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOD/(5/350))/smpls20$volfilt_mL*1000) ] <- 3 #if above LOD

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) ] <- 4 #if above LOQ

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1] <- 5 #if above LOQ, and within 1-10 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=10] <- 6 #if above LOQ, and within 10-100 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=100] <- 7 #if above LOQ, and within 100-1000 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1000] <- 8 #if above LOQ, and within 1e3-1e4 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1E4] <- 9 #if above LOQ, and within 1e4-1e5 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1E5] <- 10 #if above LOQ, and within 1e5-1e6 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1E6] <- 11 #if above LOQ, and within 1e6-1e7 copies per L

smpls20$no_for_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                                 log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                                 smpls20$copies_per_Lwater>=1E7] <- 12 #if above LOQ, and within 1e7-1e8 copies per L

#unique(smpls20$no_for_log.10_eDNAlvls)
#add an empty column with just NAs to fil with evaluations
smpls20[,"col_log.10_eDNAlvls"] <- NA
#replace in the empty column, the order is important, 
#as you otherwise will end up with the last evaluations
smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L==0 ] <- 0 #if No Ct

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L<=
                              log10((smpls20$LOD/(5/350))/smpls20$volfilt_mL*1000) 
                            & smpls20$log.10_copies_L>0] <- 0 #if below LOD but above zero

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOD/(5/350))/smpls20$volfilt_mL*1000) ] <- 0 #if above LOD

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) ] <- 1 #if above LOQ

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1] <- 2 #if above LOQ, and within 1-10 copies per

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=10] <- 3 #if above LOQ, and within 10-100 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=100] <- 4 #if above LOQ, and within 100-1000 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1000] <- 5 #if above LOQ, and within 1e3-1e4 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1E4] <- 6 #if above LOQ, and within 1e4-1e5 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1E5] <- 7 #if above LOQ, and within 1e5-1e6 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1E6] <- 8 #if above LOQ, and within 1e6-1e7 copies per L

smpls20$col_log.10_eDNAlvls[smpls20$log.10_copies_L>=
                              log10((smpls20$LOQ/(5/350))/smpls20$volfilt_mL*1000) &
                              smpls20$copies_per_Lwater>=1E7] <- 9 #if above LOQ, and within 1e7-1e8 copies per L
#check unique values
maxcolval <- max(unique(smpls20$col_log.10_eDNAlvls))
# see this website for how to make a color ramp,
#with specified steps:
#https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
colfunc <- colorRampPalette(c("white", "green","black"))
#colfunc(maxcolval)
#plot(rep(1,maxcolval),col=colfunc(maxcolval),pch=19,cex=3)
#make a sequence of numbers
col.no <-seq(1:maxcolval)
#put the colors in to a variable
col.val <-colfunc(maxcolval)
#bind both and turn in to a data frame
colgreen.f.abLOQ <- as.data.frame(cbind(col.no, col.val))
#match to data frame
smpls20$col.f.green.hexcol.ab.LOQ <- colgreen.f.abLOQ$col.val[match(smpls20$col_log.10_eDNAlvls, colgreen.f.abLOQ$col.no)]
#make the colors characters, to be able to replace zeros
smpls20$col.f.green.hexcol.ab.LOQ <- as.character(smpls20$col.f.green.hexcol.ab.LOQ)
#replace NAs with the transparent color defined previously
smpls20$col.f.green.hexcol.ab.LOQ[is.na(smpls20$col.f.green.hexcol.ab.LOQ)]<-0
#add an empty column with just NAs to fil with color codings
smpls20[,"eDNA_eval_t_repl_col"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
smpls20$eDNA_eval_t_repl_col[smpls20$eDNA_eval_p_repl_col==1] <- "white" #"white" #"NoCt" #0  
smpls20$eDNA_eval_t_repl_col[smpls20$eDNA_eval_p_repl_col==2] <- "yellow" #"yellow" #"belowLOD" #1  
smpls20$eDNA_eval_t_repl_col[smpls20$eDNA_eval_p_repl_col==3] <- "orange" # "azure3" #"AbLOD_BeLOQ" #2 
smpls20$eDNA_eval_t_repl_col[smpls20$eDNA_eval_p_repl_col==4] <- "red" #"azure4" #"one aboveLOQ" #3 
smpls20$eDNA_eval_t_repl_col[smpls20$eDNA_eval_p_repl_col==5] <- "black" #"black" #"all 3 above LOQ" #4

#write csv
#define path to where to write the file
wd00_10 <- paste0(wd00,wd10)
#write out a csv-file
write.csv(smpls20, file = paste0(wd00_10,"/","suppmatr_10.05d_MONIS4_eDNA_smpls20.csv"))





