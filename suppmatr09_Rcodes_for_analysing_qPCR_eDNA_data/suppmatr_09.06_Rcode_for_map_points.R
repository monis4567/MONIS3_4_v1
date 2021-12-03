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

#define path and file name for csv file to read in
in.csv.file01 <- paste(wd00,wd10,"/suppmatr_10.05d_MONIS4_eDNA_smpls20.csv",sep="")
in.csv.file02 <- paste(wd00,wd10,"/suppmatr_10.05e_MONIS4_eDNA_smpls06.csv",sep="")
#read in csv file from previous code
smpls20 <-as.data.frame(read.csv(in.csv.file01,
                                   header = TRUE, sep = ",", quote = "\"",
                                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))
#read in species names
scpnmames <-as.data.frame(read.csv("suppmatr_09.01_assay_no_to_spcnames_03.csv",
                                   header = TRUE, sep = ",", quote = "\"",
                                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))

#read in csv file from previous code
smpls06 <-as.data.frame(read.csv(in.csv.file02,
                                 header = TRUE, sep = ",", quote = "\"",
                                 dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))


harbours <-as.data.frame(read.csv("suppmatr_09.02_MONIS3_harbour_and_pos_water_samples06.csv",
                                  header = TRUE, sep = ",", quote = "\"",
                                  dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))


#################################################################################
#  plot eDNA copies per L on map for season per species
#################################################################################

# #get package for adding pies and bars on the map
if(!require(mapplots)){
  install.packages("mapplots")
  library(mapplots)
}
#install.packages("mapplots")
library(mapplots)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
if(!require(mapdata)){
  install.packages("mapdata")
  library(mapdata)
}
#install.packages("mapdata")
library(mapdata)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
if(!require(maps)){
  install.packages("maps")
  library(maps)
}
library(maps)

#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
if(!require(TeachingDemos)){
  install.packages("TeachingDemos")
  library(TeachingDemos)
}
library(TeachingDemos)

#get package to make maps
#install.packages("rworldmap")
if(!require(rworldmap)){
  install.packages("rworldmap")
  library(rworldmap)
}
require (rworldmap)

#get another package to make maps
#install.packages("rworldxtra")
if(!require(rworldxtra)){
  install.packages("rworldxtra")
  library(rworldxtra)
}
require(rworldxtra)

## install the package 'scales', 
#which will allow you to make points on your plot more transparent
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

## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
if(!require(marmap)){
  install.packages("marmap")
  library(marmap)
}
library(marmap)
#https://stackoverflow.com/questions/26279668/getnoaa-bathy-noaa-server-down
#sessionInfo()
packageVersion("marmap")
## draw a map of the world, but limit the x-axis to between 8 to 14 ° E lon 
## and the y-axis to between 54 and 58 N° lat , 
## colour the land and sea in hexadecimal colors
## import bathymetric data for the region, 
## with lon1 and lon2 specifying the western end eastern 
## boundaries of the bathymetric plot 
## set the resolution to be 2, a higher resulotion takes longer to download

# if the get 'marmap::getNOAA.bathy' function fails
# it might help reinstalling the marmap package, and restarting
#the r session, and try again to get the marmap::getNOAA.bathy 
#function working
#install.packages("marmap")
#check if the object with the bathymetric map does not exists

#check if the object with the bathymetric map does not exists
if (!exists("dk.sea"))
{
  ## draw a map of the world, but limit the x-axis to between 8 to 14 ° E lon 
  ## and the y-axis to between 54 and 58 N° lat , 
  ## colour the land and sea in hexadecimal colors
  ## import bathymetric data for the region, 
  ## with lon1 and lon2 specifying the western end eastern 
  ## boundaries of the bathymetric plot 
  ## set the resolution to be 2, a higher resulotion takes longer to download
  # All maps are to be based on the same bathymetric map , so this can be reused
  dk.sea <- marmap::getNOAA.bathy(lon1 = 6, lon2 = 18,
                                  lat1 = 54, lat2 = 58, resolution = 2)
  
  #close the if test above - i.e. if the 'dk.sea' object does not exist, then get it
}

# All maps are to be based on the same bathymetric map , so this can be reused
#dk.sea <- getNOAA.bathy(lon1 = 6, lon2 = 14,
#                        lat1 = 54, lat2 = 58, resolution = 2)



#dk.sea <- getNOAA.bathy(lon1 = 6, lon2 = 14,
#                        lat1 = 54, lat2 = 58, resolution = 2)
########################################################################################
#
# prepare maps with eDNA categories mapped on harbours
# notice the section in the middle, that allows for plotting bars on the harbours
# to reflect the intensity of the eDNA levels. This middle section is currently
# commented out!
#
########################################################################################
#Make a transparent color
transp_col <- rgb(0, 0, 0, 0)
#first get the species names, and Assay No's and assign numbers to each, to use 
# for numbering appendices
#get the unique species names
latspecnm <- unique(smpls20$spc)

#match the assay number to the data frame with species
AIfps <- scpnmames$AssayIDNo[match(latspecnm, scpnmames$gen_specnm)]
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
#get unique latin species names from the data frame
#latspecnm <- unique(smpls20$spc)
#delete a selected element from the list
#latspecnm <- latspecnm[latspecnm != "Cordylophora_caspia"]

#latspecnm <- "Acipenser_baerii"
#latspecnm <- "Bonnemaisonia_hamifera"
#latspecnm <- "Carassius_auratus"
#latspecnm <- "Colpomenia_peregrine"
#latspecnm <- "Cordylophora_caspia"
#latspecnm <- "Crassostrea_gigas"
#latspecnm <- "Cyprinus_carpio"
#latspecnm <- "Eriocheir_sinensis"
#latspecnm <- "Homarus_americanus"
#latspecnm <- "Karenia_mikimotoi"
#latspecnm <- "Mnemiopsis_leidyi"
#latspecnm <- "Mya_arenaria"
#latspecnm <- "Neogobius_melanostomus"
#latspecnm <- "Oncorhynchus_mykiss"
#latspecnm <- "Oncorhyncus_gorbuscha"
#latspecnm <- "Paralithodes_camtschaticus"
#latspecnm <- "Prorocentrum_minimum"
#latspecnm <- "Pseudochattonella_farcimen"
#latspecnm <- "Pseudochattonella_serruculata"
#latspecnm <- "Rhithropanopeus_harrisii"

#latspecnm <- "Colpomenia_peregrine"
#latspecnm <- "Cordylophora_caspia"
#latspecnm <- "Cyprinus_carpio"

#remove NA from the factors
#see this webpage: https://www.r-bloggers.com/r-drop-factor-levels-in-a-dataset/
latspecnm <- latspecnm[latspecnm!="NA"]
latspecnm <- latspecnm[latspecnm!="Homarus_americanus"]
latspecnm <- latspecnm[latspecnm!="Paralithodes_camtschaticus"]
# loop over all species names in the unique list of species, and make plots. 
#Notice that the curly bracket ends after the pdf file is closed
for (spec.lat in latspecnm){
  #print(spec.lat)
  #}
  
  #get the Danish commom name
  sbs.dk.nm <- scpnmames$dk_comnm[match(spec.lat, scpnmames$gen_specnm)]
  
  #get the abbreviated name
  sbs.abbrnm <- scpnmames$six_lett_spec_abbrv[match(spec.lat, scpnmames$gen_specnm)]
  #make the species name in italics
  #ital.spec.lat <- bquote(''~italic(.(spec.lat)))
  #get the AssayIDNo
  AIfps <- scpnmames$AssayIDNo[match(spec.lat, scpnmames$gen_specnm)]
  #pad with zero
  sbs.AssIDNo <-stringr::str_pad(AIfps, 2, pad = "0")
  
  #get the number for the appendix plot number
  no.spc.app.plot <- nlspnm$no.latspc[match(spec.lat, nlspnm$latspecnm)]
  #subset based on variable values, subset by species name
  sbs.smpls20 <- smpls20[ which(smpls20$spc==spec.lat), ]
  #count using the plyr-package - see: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
  sbs.tot_smpl <- dplyr::count(sbs.smpls20, Harbour, declon, declat)
  
  maxcol.log10.val.eDNA <- max(sbs.smpls20$col_log.10_eDNAlvls)
  #rearrange the data fram using the reshape function
  sbs.smpls21<-reshape(sbs.smpls20,timevar="season",idvar="Harbour",direction="wide")
  #subset based on variable values
  # subset among the seasons
  sbs.smpls20.for <- sbs.smpls20[ which(sbs.smpls20$season=="foraar" ), ]
  sbs.smpls20.eft <- sbs.smpls20[ which(sbs.smpls20$season=="efteraar" ), ]
  
  #XXXXX______begin plot w squares  on map ________XXXX
  #______________________________________________________________________________________
  # set to save plot as pdf file with dimensions 8.26 to 2.9
  # 8.26 inches and 2.9 inhes equals 210 mm and 74.25 mm
  # and 210 mm and 74.25 mm matches 1/4 of a A4 page
  pdf(c(paste(wd00,wd10,"/suppmatr_10.06a_App_B_AssNo",
              sbs.AssIDNo,"_",sbs.abbrnm,".pdf",  sep = ""))
      ,width=(1.6*8.2677),height=(1.6*2*2.9232))
  #try saving as jpeg instead - then comment out the 'pdf' part above
  # jpeg(c(paste(wd00,wd10,"/suppmatr_10.06a_App_B_AssNo",
  #              sbs.AssIDNo,"_",sbs.abbr.nm,".jpg",  sep = ""))
  #      ,width=(1.6*8.2677),height=(2*1.6*2*2.9232),
  #      units="in",res=300,pointsize=16)
  #factors to multiply radius on each bar
  fct1 <- 1.000 
  fct2 <- 0.08
  
  ################################################################################
  # uncomment section below to get bathymetric isobars on maps.
  ################################################################################
  ##plot bathymetric map
  
  #plot(dk.sea,  add=F, lwd = c(0, 0), col=transp_col, image = TRUE, bpal = c(alpha(blues, 0.4)),
  #     xlim = c(6, 14), ylim = c(54, 58),
  #     asp=1.4, #change from 1 to 1.6 for stretchin the map along the latitude
  #     cex=2.8, #vfont = c("sans serif", "plain"),
  #     las=1
  #)
  
  # plot white colored bathymetry
  plot(dk.sea,  add=F, lwd = c(0, 0), col=transp_col, image = TRUE, bpal = "white",
       xlim = c(6, 14), ylim = c(54, 58),
       asp=1.4, #change from 1 to 1.6 for stretchin the map along the latitude
       cex=2.8, #vfont = c("sans serif", "plain"),
       las=1
  )
  #plot(dk.sea, add=TRUE, lwd = c(0.8, 1.6), lty = c(1, 1),
  #     xlim = c(6, 14), ylim = c(54, 58),
  #     deep = c(-500, 0), shallow = c(-20, 0), step = c(20, 0),
  #     cex=2.8, vfont = c("sans serif", "plain"),
  #     asp=1.4, #change from 1 to 1.6 for stretchin the map along the latitude
  #     col = c("#00009B", "black"), drawlabels = c(TRUE, TRUE),
  #     las=1) # set labels horizontal to axis : see : https://stackoverflow.com/questions/1828742/rotating-axis-labels-in-r
  
  #plot land on map
  map('worldHires', add=TRUE, fill=TRUE, 
      xlim = c(6, 14), ylim = c(54, 58),
      
      #col="#11263D",
      col="grey",
      asp=1.4, #change from 1 to 1.6 for stretchin the map along the latitude
      bg=transp_col,
      las=1)
  
  ####################################################################################
  # start #add bars to positions on the map
  ####################################################################################
  
  # colnames(sbs.smpls21) 
  # #for row in data frame add a bar for each lon-lat position
  # for (i in 1:nrow(sbs.smpls21))
  #   subplot(barplot(height= 
  #                     fct1*as.numeric(
  #                       as.vector
  #                       (unlist(sbs.smpls21[i,
  #                       c("col_log.10_eDNAlvls.foraar",
  #                       "col_log.10_eDNAlvls.efteraar")],use.names=F)
  #                     )), 
  #                   axes=F, add=F, pos=1, 
  #                   #beside = T,
  #                   col=c(  alpha(c(
  #                     "blue", #for foraar
  #                     "red")   #for efteraar
  #                     , 0.6)
  #                   )
  #                   ,
  #                   #density = (10), angle = 0, #make the fill in bars texture
  #                   #lwd = 4,
  #                   ylim=range(c(0,1))),
  #           #specify lon lat to place bar
  #           x=sbs.smpls21[i, 'declon.foraar'], 
  #           y=sbs.smpls21[i, 'declat.foraar'],
  #           #border=c("orange", #for foraar
  #           #          "brown"   #for efteraar
  #           #),
  #           size=c(0.4, fct2)
  #   )
  # 
  # 
  # #for row in data frame add a bar for each lon-lat position
  # # that is repeat the bars plotted above
  # #this time add texture instead of colors
  # for (i in 1:nrow(sbs.smpls21))
  #   subplot(barplot(height= 
  #                     fct1*as.numeric( 
  #                       as.vector(
  #                         unlist(sbs.smpls21[i,
  #                         c("col_log.10_eDNAlvls.foraar",
  #                         "col_log.10_eDNAlvls.efteraar")],use.names=F)
  #                     )), 
  #                   axes=F, add=F, pos=1, 
  #                   #beside = T,
  #                   col=c(  alpha(c(
  #                     "black", #for foraar
  #                     "black")   #for efteraar
  #                     , 0.6)
  #                   )
  #                   ,
  #                   density = (fct1*10), angle = 0, #make the fill in bars texture
  #                   lwd = 4,
  #                   ylim=range(c(0,1))),
  #           #specify lon lat to place bar
  #           x=sbs.smpls21[i, 'declon.foraar'], 
  #           y=sbs.smpls21[i, 'declat.foraar'],
  #           #border=c("orange", #for foraar
  #           #          "brown"   #for efteraar
  #           #),
  #           size=c(0.4, fct2)
  #   )
  # 
  # 
  ####################################################################################
  # end #add bars to positions on the map
  ####################################################################################
  
  
  #add text to the same lon lat position for harbour
  #deduct a bit from the latitude, to lower the positioning of the label
  text(sbs.smpls21$declon.foraar, 
       sbs.smpls21$declat.foraar-0.18, 
       labels=sbs.smpls21$Harbour, 
       pos=1,
       cex= 1.2
  )
  #add a point to each position for foraar
  #for eDNA evaluation
  points (sbs.smpls21$declon.foraar-0.05, 
          sbs.smpls21$declat.foraar-0.08, 
          pch = 22, 
          bg=c(alpha(c(as.character(sbs.smpls21$eDNA_eval_t_repl_col.foraar)))),
          col="blue", #set color of point
          lwd=2,
          pos = 1,
          cex= 2.4)
  
  #add a point to each position for efteraar
  #for eDNA evaluation
  points (sbs.smpls21$declon.efteraar+0.05, 
          sbs.smpls21$declat.efteraar-0.08, 
          pch = 22, 
          bg=c(alpha(c(as.character(sbs.smpls21$eDNA_eval_t_repl_col.efteraar)))),
          col="red", #set color of point
          lwd=2,
          pos = 1,
          cex= 2.4)
  
  #add a point to each position for conventional monitoring
  #
  points (sbs.smpls21$declon.efteraar, 
          sbs.smpls21$declat.efteraar-0.16, 
          pch = 25, 
          bg=c(alpha(c(as.character(sbs.smpls21$col.f.conv_rec_val.efteraar)))),
          col="black", #set color of point
          lwd=2,
          pos = 1,
          cex= 1.6)
  #add a title
  #title(main = c(paste("eDNA detected from ",spec.lat,"-\n AssayNo ",sbs.AssIDNo,"(",sbs.dk.nm,")")
  #              , sep=""))
  spec.lat.no_undersc <- paste(sub('_', ' ', spec.lat))
  #add a title with bquote
  title(main=c(bquote('eDNA detected from'
                      ~italic(.(spec.lat.no_undersc))
                      ~'('
                      ~.(sbs.dk.nm)
                      ~'), Assay Id No:'~.(sbs.AssIDNo)
  )))
  #title(main = c(bquote('examp'~italic(T)~'zzz'~italic(T))))
  # add legend for spring and fall
  legend("topleft", "(x,y)", 
         bg="white",
         c("spring", "autumn"),
         ncol=1,
         pch = c(22,22), #set type of point
         col= c("blue", "red"), #set color of point
         #pt.bg=c(alpha(c("white", "white"), 0.6)), #set background color of point
         pt.bg=c(c("white", "white")), #set background color of point
         pt.lwd=c(1.2),      
         title = "season ",
         cex=1.1,
         inset = 0.02)
  
  # add legend for eDNA evaluation
  legend("left", "(x,y)", 
         bg="white",
         c("No Ct","below LOD", "above LOD and below LOQ" ,"1 above LOQ", "3 above LOQ"),
         ncol=1,
         pch = c(22,22, 22, 22, 22), #set type of point
         col= c("black", "black", "black", "black", "black"), #set color of point
         #pt.bg=c(alpha(c("white", "yellow", "black"), 0.6)), #set background color of point
         pt.bg=c(c("white", "yellow", "orange","red", "black")), #set background color of point
         pt.lwd=c(1.0),      
         title = "eDNA evalution ",
         cex=1.1,
         inset = 0.02)
  
  
  # add legend for conventional monitoring  evaluation
  legend("bottomleft", "(x,y)", 
         bg="white",
         c("Unknown, no previous record", "Recorded in the past","Recorded during MONIS4"),
         ncol=1,
         pch = c(25,25, 25), #set type of point
         col= c("black", "black", "black"), #set color of point
         #pt.bg=c(alpha(c("white", "red", "black"), 0.6)), #set background color of point
         pt.bg=c(c("white", "red", "black")), #set background color of point
         pt.lwd=c(1.0),      
         title = "conventional monitoring",
         cex=1.1,
         inset = 0.02)
  fct3=10*0.65
  
  ################################################################################
  
  # # add legend for size of bars
  # legend("topleft", "(x,y)", 
  #        #       bg=transp_col, 
  #        bg="white", 
  #        c("10-100 copies/L", 
  #          "100-1000 copies/L", 
  #          "1E3-1E4 copies/L", 
  #          "1E4-1E5 copies/L", 
  #          "1E5-1E6 copies/L", 
  #          "1E6-1E7 copies/L", 
  #          "max-copies"),
  #        ncol=1, 
  #        #pch = c(22,22, 22), #set type of point
  #        lty = c(1, 1, 1, 1, 1, 1, 1),
  #        lwd = c(6, 6, 6, 6, 6, 6, 6),
  #        col= c("black", "black", "black", "black", "black", "black","black"), #set color of point
  #        pt.bg=c(alpha(c("green", "green", "green", "green", "green", "green", "green"), 0.6)), #set background color of point
  #        
  #        seg.len=c(
  #          fct3*fct1*fct2*(3),
  #          fct3*fct1*fct2*(4),
  #          fct3*fct1*fct2*(5),
  #          fct3*fct1*fct2*(6),
  #          fct3*fct1*fct2*(7),
  #          fct3*fct1*fct2*(8),
  #          fct3*fct1*fct2*(maxcol.log10.val.eDNA)
  #          
  #        ),
  #        #       title = "catch (kg)",
  #        cex=1.0,
  #        inset = 0.05)
  ################################################################################
  
  # #add a title for the pdf plot
  # mtext(c(paste("Appendix B",no.spc.app.plot,"."),  sep = ""), outer=TRUE, 
  #       #use at , adj and padj to adjust the positioning
  #       #at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
  #       adj=0.02,#1,
  #       padj=0,#2,
  #       #use side to place it in the top
  #       side=3, cex=1.6, line=-2.15)
  
  
  
  #add a title for the pdf plot
  mtext(c(paste("suppmatr_10.05b_Appendix B",no.spc.app.plot,"."),  sep = ""), outer=TRUE, 
        #use at , adj and padj to adjust the positioning
        #at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
        adj=0.02,#1,
        padj=0,#2,
        #use side to place it in the top
        side=3, cex=1.6, line=-2.15)
  
  
  
  # end the pdf-file to save as 
  dev.off()
  #below is the end of the loop initiated above
}
# here the loop over all species names ends
####################################################################################







#################################################################################
#  plot eDNA copies per L on map part 2
#   plot eDNA copies per L on map for season for multiple species
#################################################################################

#number of assays
noassays <- length(unique(smpls20$AssayIDNo))
ass.nos <- unique(smpls20$AssayIDNo)
ass.nos <- data.frame(ass.nos)
#install.packages('dplyr')
#install.packages("rworldxtra")
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
#add a count for each line and turn in to a dataframe 
as2 <- ass.nos %>% dplyr::group_by(ass.nos) %>% dplyr::mutate(id = row_number())
as3 <- data.frame(as2)
#sort using order
as3 <- as3[order(ass.nos),]
#add a count for each line and turn in to a dataframe 
as4 <- as3 %>% dplyr::group_by(as3$id) %>% dplyr::mutate(id = row_number())
as5 <- data.frame(as4)

#divide the number of assays with a number
non <- noassays/4
#add a column w NAs
as5$sh1 <- "NA"
#Assign values to the NA column
as5$sh1[(0*non+1):(1*non)] <- as5$as3.id[(0*non+1):(1*non)]+0
as5$sh1[(1*non+1):(2*non)] <- as5$as3.id[(1*non+1):(2*non)]+1
as5$sh1[(2*non+1):(3*non)] <- as5$as3.id[(2*non+1):(3*non)]+2
as5$sh1[(3*non+1):(4*non)] <- as5$as3.id[(3*non+1):(4*non)]+3
#add a count for each line
as5 %>% dplyr::group_by(as5$sh1) %>% dplyr::mutate(id = row_number())
#add a count for each line
as6 <- as5 %>% dplyr::group_by(sh1) %>% dplyr::mutate(id = row_number())
#turn into a dataframe
as6 <- data.frame(as6)
#add back to dataframe as corrections to latitude and longitude
as6$lon_rct <- as.numeric(as6$sh1)
as6$lat_rct <- as.numeric(as6$id)

#match with AssayIDNo
smpls20$lon_rct <- as6$lon_rct[match(smpls20$AssayIDNo, as6$ass.nos)]
smpls20$lat_rct <- as6$lat_rct[match(smpls20$AssayIDNo, as6$ass.nos)]
#smpls20$spc.season
#colnames(smpls20)
spcls01 <- data.frame(unique(smpls20$spc))
colnames(spcls01)<-c("spc")
sp_phyl01 <- c("Genus_species","Phylum")
sp_phyl02 <- c("Acipenser_baerii","Chordata")
sp_phyl03 <- c("Bonnemaisonia_hamifera","Rhodophyta")
sp_phyl04 <- c("Carassius_auratus","Chordata")
sp_phyl05 <- c("Colpomenia_peregrine","Ochrophyta")
sp_phyl06 <- c("Cordylophora_caspia","Cnidaria")
sp_phyl07 <- c("Crassostrea_gigas","Mollusca")
sp_phyl08 <- c("Cyprinus_carpio","Chordata")
sp_phyl09 <- c("Eriocheir_sinensis","Arthropoda")
sp_phyl10 <- c("Homarus_americanus","Arthropoda")
sp_phyl11 <- c("Karenia_mikimotoi","Dinoflagellata")
sp_phyl12 <- c("Mnemiopsis_leidyi","Ctenophora")
sp_phyl13 <- c("Mya_arenaria","Mollusca")
sp_phyl14 <- c("Neogobius_melanostomus","Chordata")
sp_phyl15 <- c("Oncorhynchus_mykiss","Chordata")
sp_phyl16 <- c("Oncorhyncus_gorbuscha","Chordata")
sp_phyl17 <- c("Paralithodes_camtschaticus","Arthropoda")
sp_phyl18 <- c("Prorocentrum_minimum","Dinoflagellata")
sp_phyl19 <- c("Pseudochattonella_farcimen","Heterokontophyta")
sp_phyl20 <- c("Pseudochattonella_verruculosa","Heterokontophyta")
sp_phyl21 <- c("Rhithropanopeus_harrisii","Arthropoda")

spc_phyl <- data.frame(cbind(sp_phyl01, sp_phyl02, sp_phyl03, sp_phyl04, sp_phyl05, sp_phyl06,
                             sp_phyl07, sp_phyl08, sp_phyl09, sp_phyl10, sp_phyl11, sp_phyl12, 
                             sp_phyl13, sp_phyl14, sp_phyl15, sp_phyl16, sp_phyl17, sp_phyl18, 
                             sp_phyl19, sp_phyl20, sp_phyl21))
spc_phyl <- data.frame(t(spc_phyl))
colnames(spc_phyl) <- c("gen_spc","phylum")
#match with dataframe with species in main dataframe
spcls01$phylum <- spc_phyl$phylum[match(spcls01$spc, spc_phyl$gen_spc)]
#match with dataframe with species in main dataframe
smpls20$phylum <- spcls01$phylum[match(smpls20$spc, spcls01$spc)]
#unique phylum in data frame
u.phy <- unique(smpls20$phylum)

####################################################################################
#
# Prepare a dataframe for ipdw plots
#
####################################################################################
#make a copy of the df
smpls23 <- smpls06
#paste columns together
smpls23$season.Harbour <- paste(smpls23$season, smpls23$Harbour,sep=".")
harbours$Saeson.Harbour <- paste(harbours$Saeson,harbours$Harbour,sep=".")
#use newly pasted columns to match between data frames
smpls23$volfilt_mL  <- harbours$volfilt_mL[match(smpls23$season.Harbour,harbours$Saeson.Harbour)]
#match columns in a previously loaded dataframe and add as new columns in this dataframe
smpls23$Phylum <- scpnmames$Phylum[match(smpls23$spc, scpnmames$gen_specnm)]
smpls23$Class <- scpnmames$Class[match(smpls23$spc, scpnmames$gen_specnm)]
smpls23$Order  <- scpnmames$Order[match(smpls23$spc, scpnmames$gen_specnm)]
smpls23$Family   <- scpnmames$Family[match(smpls23$spc, scpnmames$gen_specnm)]
smpls23$col_f_Phyl   <- scpnmames$col_f_Phyl[match(smpls23$spc, scpnmames$gen_specnm)]
smpls23$spcf_col_f_Phyl   <- scpnmames$spcf_col_f_Phyl[match(smpls23$spc, scpnmames$gen_specnm)]
#make a new column w copies per L
smpls23$volfilt_L <- smpls23$volfilt_mL/1000
#colnames(smpls23)
#unique(smpls23$spc)
#copy a column to a new column
smpls23$m.q.copies01 <- as.numeric(smpls23$meanQuantitycopies)
#if value in this new column is NA, then make it 1 instead
#this will make it possible to log10-plot no measured eDNA-levels as zero
smpls23$m.q.copies01[is.na(smpls23$m.q.copies01)] <- 1
#replace all values below zero with 1, 
#to make it possible to log10-plot no measured eDNA-levels as zero
smpls23$m.q.copies01[smpls23$m.q.copies01 < 0] <- 1
#put this column in a new column
smpls23$m.q.copies02 <- smpls23$m.q.copies01
#Replace all copy levels below LOQ with 1
smpls23$m.q.copies02[smpls23$m.q.copies01 < smpls23$LOQ] <- 1
#smpls23$m.q.copies02

#Using the copy per L water filtered calculation 
#from Knudsen et al. (2019) Journ. of Exp. Marine Biol. and Ecol.
#(supp. information, setup 05)

#volume of filtered seawater (a: "volfilt_mL" /1000)
#re-suspended volume of purified DNA (b: 350 μL) 
#the amount of purified DNA added to each qPCR-reaction volume (c: 5 μL/qPCR reaction)
#measured copy number in the qPCR-reaction, calculated by the MxPro software based 
#on the standard curve (Y, copies/qPCR reaction = smpls06$meanQuantitycopies

#X (copies/L) = (Y·b)/(c·a) = Y·(350/(5·"volfilt_L")) 
smpls23$copy.per.L = smpls23$m.q.copies02*(350/(5*smpls23$volfilt_L))
#Replace  with 1 , whenever the copy number is too low
smpls23$copy.per.L[smpls23$m.q.copies01 < smpls23$LOQ] <- 1
#smpls23$copy.per.L

#add an empty column with just NAs to fil with color codings
library(dplyr)
# smpls23 %>% 
#   mutate(eDNA_eval_season = paste(season,".",eDNA_col_eval_mean, sep=""))
smpls24 <- smpls23 %>% 
  mutate(eDNA_eval_season = paste(season,".",eDNA_col_eval_mean, sep=""))
#smpls24 <- smpls23
head(harbours,4)
head(smpls24,4)
#match between data frames
smpls24$Coll_date <- harbours$Coll_date[match(smpls24$season.Harbour,harbours$Saeson.Harbour)]
smpls24$declat <- harbours$declat[match(smpls24$season.Harbour,harbours$Saeson.Harbour)]
smpls24$declon <- harbours$declon[match(smpls24$season.Harbour,harbours$Saeson.Harbour)]
smpls24$EuroFinsampleno <- harbours$EuroFinsampleno[match(smpls24$season.Harbour,harbours$Saeson.Harbour)]
#get year, month and day
smpls24$year <- gsub("\\..*","",smpls24$Coll_date)
smpls24$day <- gsub("*.*\\.","",smpls24$Coll_date)
smpls24$month <- data.frame(do.call('rbind', strsplit(as.character(smpls24$Coll_date),'.',fixed=TRUE)))[,2]
# set a new working directory
setwd(wd00)
#define output file and path
outfile01 <- paste(wd00,wd10,"/suppmatr_10.06b_MONIS4_eDNA_smpls24.csv",sep="")
#write the table to a csv
#to be used for the next R-code that plots eDNA levels on maps
write.csv(smpls24, file = outfile01)
#set the working dir back agian
#setwd(curr.wd)
