#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-


#################################################################################
# Code developed by Steen W. Knudsen at NIVA-DK

# The 20 non indeginuous marine species targeted for analysis are the same 20 
# species that the MONIS2, MONIS3 and MONIS4 project, carried out at NIVA-DK
#
# The overall purpose of the present code is to analyse the eDNA levels inferred
# in the qPCR setups performed over 2017-2018.
#################################################################################
#remove everything in the working environment, 
#without a warning!!
rm(list=ls())


wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2020/MS_eDNA_MONIS3_4/MONIS3_4_v1"
# set working directory
wd10 <- "suppmatr10_plots_and_tables_from_Rcode"
wd09 <- "suppmatr09_Rcodes_for_analysing_qPCR_eDNA_data"
wd08 <- "suppmatr08_merged_qPCRdata_txt_reports"

inf01 <- "suppmatr_09.03_MONIS4_inv_spc_distr_tradit_survey_meth_2018jun14_04.csv"
#paste dirs together
wd00_wd09 <- paste0(wd00,"/",wd09)
wd00_wd10 <- paste0(wd00,"/",wd10)
wd00_wd09_inf01 <- paste0(wd00_wd09,"/",inf01)
setwd(wd00_wd10)
getwd()
#read in a table with conventional monitoring
df_c01 <- read.table(wd00_wd09_inf01,header=T, stringsAsFactors = F, sep=",")
#remove column
df_c01$spcNo <- NULL
#define harbour names
hrbnms <- c("AalborgHavn",
"Aalborgportland",
"Aarhus",
"Esbjerg",
"Fredericia",
"Frederikshavn",
"Gedser",
"Grenaa",
"Helsingoer",
"Hirtshals",
"Kalundborg",
"KalundborgStatiolHavn",
"Koebenhavn",
"Koege",
"Odense",
"Roedby")
#define abbreviations
hrb_abbr <- c("ALH",
"ALP",
"ARH",
"ESB",
"FRC",
"FRH",
"GED",
"GRE",
"HEL",
"HIR",
"KAB",
"KSH",
"KBH",
"KGE",
"ODE",
"ROD")
# bind columns to a dataframe
df_ab01 <- as.data.frame(cbind(hrbnms,hrb_abbr))
#use this to replace column names
colnames(df_c01)[-1] <- df_ab01$hrb_abbr[match(colnames(df_c01)[-1],df_ab01$hrbnms)]

#define input file
inf02 <- "suppmatr_09.23_inp_conv_monit.csv"
#paste path and file name together
wd00_wd09_inf02 <- paste0(wd00_wd09,"/",inf02)
#read in file
df_ct02 <- read.table(wd00_wd09_inf02,stringsAsFactors = F,header = T,sep=",")
#remove species number column
df_ct02$spcNo <- NULL
#use df to replace column names
colnames(df_ct02)[-1] <- df_ab01$hrb_abbr[match(colnames(df_ct02)[-1],df_ab01$hrbnms)]
#get commentaries on rows
comr <- tail(df_ct02, n =1)
#delete the last row
n<-dim(df_ct02)[1]
df_ct03<-df_ct02[1:(n-1),]#
nrwdf03 <- nrow(df_ct03)

df_ct04 <- df_ct03[1:nrwdf03,-1]
#https://stackoverflow.com/questions/16516593/convert-from-lowercase-to-uppercase-all-values-in-all-character-variables-in-dat
df_ct04 <- data.frame(lapply(df_ct04, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))
df_ct03[1:nrwdf03,-1] <- df_ct04
df_ct05 <- df_ct03
df_ct05 <-df_ct05[!grepl("Acipens",df_ct05$Species_name),]

df_ct05 <- df_ct05[!grepl("Paralith",df_ct05$Species_name),]
df_ct05 <- df_ct05[!grepl("Homarus",df_ct05$Species_name),]
df_ct05$Species_name <- gsub("peregrine","peregrina",df_ct05$Species_name)
df_ct05$Species_name <- gsub("Crassostrea","Magallanas",df_ct05$Species_name)
df_ct05$Species_name <- gsub("_"," ",df_ct05$Species_name)
outf01 <- "suppmatr_10.23_table_w_conv_monitoring.csv"
#paste output file and path together
wd00_wd10_outf01 <- paste0(wd00_wd10,"/",outf01)
#write out the table
write.csv(df_ct05,wd00_wd10_outf01)
#str(comr[1])
