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

#wd00 <- "/Users/steenknudsen/Documents/Documents/NIVA_Ansaettelse_2020/MS_eDNA_MONIS3_4/"
wd00 <- "/home/hal9000/MONIS3_4_v1/"
#wd00 <- "/home/hal9000/test_plot_jpg_instead_of_pdf/"
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


####################################################################################
#
# prepare std dilution curve plots for each for species
#
####################################################################################

#first get unique species names 
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
#latspecnm <- "Mya_arenaria"
#latspecnm <- "Crassostrea_gigas.foraar"
#latspecnm <- "Crassostrea_gigas.efteraar"

#define the two seasons
dk.seasons <- c("foraar","efteraar")
eng.seasons <- c("spring","autumn")
tr.seasons <-  data.frame(dk.seasons, eng.seasons)

tr.seasons
class(tr.seasons)

amp <- smpls02

######################################################################################
#   make standard curve plots for each species for each season 
######################################################################################

########################################################
# for loop start here
########################################################
#spec.lat <- "Mnemiopsis_leidyi"
#latspecnm <- "Mnemiopsis_leidyi"
# loop over all species names in the unique list of species, and make plots. 
#Notice that the curly bracket ends after the pdf file is closed
for (spec.lat in latspecnm){
  #print(spec.lat)
  #}
  
  #get the Danish commom name
  #first split the string by the dot
  #https://stackoverflow.com/questions/33683862/first-entry-from-string-split
  #and escape the dot w two backslashes
  latnm <- sapply(strsplit(spec.lat,"\\."), `[`, 1)
  sbs.dknm <- scpnmames$dk_comnm[match(latnm, scpnmames$gen_specnm)]
  #get AssIDNo
  sbs.AssIDNo <- scpnmames$AssayIDNo[match(latnm, scpnmames$gen_specnm)]
  #get Abbreviated name
  sbs.abbr.nm <- scpnmames$abbr.nm[match(latnm, scpnmames$gen_specnm)]
  #get the number for the appendix plot number
  AIfps <- nlspnm$no.latspc[match(spec.lat, nlspnm$latspecnm)]
  no.spc.app.plot <-stringr::str_pad(AIfps, 2, pad = "0")
  
  #get the latin species nam without underscore
  spec.lat.no_undersc <- paste(sub('_', ' ', spec.lat))
  #define path to where to write the file
  wd00_10 <- paste0(wd00,wd10)
  
  # # Exporting PFD files via postscript()           
  # pdf(c(paste(wd00_10,"/","suppmatr_10.04b_App_A",no.spc.app.plot,
  #             "_stddilser_",sbs.abbr.nm,".pdf",  sep = ""))
  #     ,width=(1.6*8.2677),height=(2*1.6*2*2.9232))
  
  #try saving as jpeg instead - then comment out the 'pdf' part above
  jpeg(c(paste(wd00_10,"/","suppmatr_10.04b_App_A",no.spc.app.plot,
               "_stddilser_",sbs.abbr.nm,".jpg",  sep = ""))
       ,width=(1.6*8.2677),height=(2*1.6*2*2.9232),
       units="in",res=300,pointsize=16)
  
  #op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
  
  # c(low, left, top, right))
  op <- par(mfrow=c(2,1), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
            oma=c(1,1,0,0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
            mar=c(5,5,5,5),# set the margin around each individual plot 
            adj = 0 # adjust the text 
  )
  #define the two seasons to loop over
  seasons <- c("foraar","efteraar")
  lettfs <- c("a","b")
  #make a dataframe with letters for seasons
  df_seaslet <- as.data.frame(cbind(seasons,lettfs))
  
  #loop over seasons per species
  for (seas in seasons){
    #}  
    #subset based on variable values, subset by species name and by season
    sbs.amp <- amp[ which(amp$gen_specnm==spec.lat & amp$season==seas), ]
    #match to get lettercode for subfigure letter that matches season
    letfseas <- df_seaslet$lettfs[match(seas,df_seaslet$seasons)]
    #match Danish season to English season
    #eng.seas <- tr.seasons$eng.seasons[match(seas,tr.seasons$dk.seasons)]
    # #check the season
    eng.seas <-if (seas=="foraar") {
      print("spring")
    } else {
      "autumn"
    }
    
    #identify LOD
    lod.id.df<-sbs.amp[(sbs.amp$WellType=='Standard'),]
    lod.val<-min(lod.id.df$Quantitycopies)
    #identify LOQ
    #limit the dataframe to only well type that equals standard
    zc<-sbs.amp[(sbs.amp$WellType=='Standard'),]
    #count the occurences of dilution steps - i.e. the number of succesful replicates
    #see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
    #zd<-count(zc, "WellName")
    zd <- dplyr::count(zc, Quantitycopies)
    #turn this into a dataframe
    ze<-as.data.frame(zd)
    #match the dilution step to the number of occurences -i.e. match between the two dataframes
    no.occ <- ze$n[match(zc$Quantitycopies,ze$Quantitycopies)]
    #add this column with counted occurences to the limited dataframe
    zg <- cbind.data.frame(zc,no.occ)
    #exlude all observations where less than 3 replicates amplified
    zh<-zg[(zg$no.occ>=3),]
    #get the lowest dilution step that succesfully ampllified on all 3 repliactes
    loq.val=min(zh$Quantitycopies)
    #Conditionally Remove Dataframe Rows with R
    #https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
    sbs.pamp<-sbs.amp[!(sbs.amp$WellType=='Standard' & sbs.amp$Quantitycopies<=5),]
    
    #__________________# plot1   - triangles________________________________________
    # Exporting EPS files via postscript()
    # postscript(c(paste("plot_qpcr_MONIS3_",sbs.AssIDNo,"_",spec.lat,"_std_dilution_series.eps", sep = "")),
    #             width=(1.6*8.2677),height=(2*1.6*2.9232),
    #             #family = "Arial", 
    #             paper = "special", onefile = FALSE,
    #             horizontal = FALSE)
    
    
    
    ##  Create a data frame with eDNA
    y.sbs.amp <- sbs.amp$CtdRn
    x.sbs.amp <- sbs.amp$Quantitycopies
    d.sbs.famp <- data.frame( x.sbs.amp = x.sbs.amp, y.sbs.amp = y.sbs.amp )
    
    #subset to only include the standard curve points
    # to infer the efficiency of the assay.
    
    sbs02_df <- sbs.amp[sbs.amp$WellType=="Standard", ]
    #calculate the covariance
    cov_sbs02 <- cov(sbs02_df$CtdRn, sbs02_df$Quantitycopies)
    #calculate the correlation
    cor_sbs02 <- cor(-log10(sbs02_df$Quantitycopies), sbs02_df$CtdRn)*100
    rcor_sbs02 <- round(cor_sbs02, 3)
    #get( getOption( "device" ) )()
    plot(
      y.sbs.amp ~ x.sbs.amp,
      data = d.sbs.famp,
      type = "n",
      log  = "x",
      las=1, # arrange all labels horizontal
      xaxt='n', #surpress tick labels on x-axis
      yaxt='n', #surpress tick labels on y-axis
      #main=c(paste("qPCR standard curve - for ",sbs.AssIDNo,"\n-",spec.lat,seas,"(",sbs.dknm,")"),  sep = ""), 
      
      #add a title with bquote
      # main=c(bquote('qPCR standard curve for'~italic(.(spec.lat.no_undersc))
      #               ~'('~.(sbs.dknm)~'), '
      #               ~'AssayNo'~.(sbs.AssIDNo)~', '
      #               ~.(eng.seas)
      # )),
      # #add a title with bquote
      main=c(bquote(~'('~.(letfseas)~') '
      
                    ~italic(.(spec.lat.no_undersc))
                    )),
      #offset = 2,
      #sub="sub-title",
      xlab="target-eDNA in extract. (copy/qPCR-reaction)",
      ylab="Cq",
      #xlim = c( 0.1, 1000000000 ),
      #ylim = c( 10, 50 )
      xlim = c( 0.234, 0.428*1000000000 ),
      ylim = c( 9.55, 48.446 )
      
    )
    #add labels to the points
    pos_vector <- rep(3, length(sbs.amp$Harbour))
    pos_vector[sbs.amp$Harbour %in% c("Roedby", "Aalborgportland", "KalundborgStatiolHavn")] <- 4
    pos_vector[sbs.amp$Harbour %in% c("AalborgHavn")] <- 2
    text(x.sbs.amp, y.sbs.amp, labels=sbs.amp$Harbour, cex= 0.8, pos=pos_vector, las=3)
    
    ##  Put grid lines on the plot, using a light blue color ("lightsteelblue2").
    # add horizontal lines in grid
    abline(
      h   = c( seq( 8, 48, 2 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    
    # add vertical lines in grid
    abline(
      v   = c( 
        seq( 0.1, 1, 0.1 ),
        seq( 1e+0, 1e+1, 1e+0 ),
        seq( 1e+1, 1e+2, 1e+1 ),
        seq( 1e+2, 1e+3, 1e+2 ),
        seq( 1e+3, 1e+4, 1e+3 ),
        seq( 1e+4, 1e+5, 1e+4 ), 
        seq( 1e+5, 1e+6, 1e+5 ),
        seq( 1e+6, 1e+7, 1e+6 ),
        seq( 1e+7, 1e+8, 1e+7 ),
        seq( 1e+8, 1e+9, 1e+8 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    # add line for LOQ
    abline(v=loq.val, lty=2, lwd=1, col="black")
    text(loq.val*0.7,15,"LOQ",col="black",srt=90,pos=1, font=1)
    
    # add line for LOD 
    abline(v=lod.val, lty=1, lwd=1, col="red")
    text(lod.val*0.7,22,"LOD",col="red",srt=90,pos=1, font=1)
    
    # add line for Ct-cut-off
    abline(h=seq(41,100,1000), lty=1, lwd=3, col="darkgray")
    text(10,40.6,"cut-off",col="darkgray",srt=0,pos=3, font=2, cex=1.2)
    
    # make a transparent color
    #transp_col <- rgb(0, 0, 0, 0)
    #make numbers for the sample type
    #convert NAs to a number 
    # https://stackoverflow.com/questions/27195956/convert-na-into-a-factor-level
    #sbs.amp.stndnm <- addNA(sbs.amp$Harbour)
    #col.01<-as.numeric(as.factor(sbs.amp.stndnm))
    #make a small dataframe w harbours and standards and numbers assigned, 
    #check that the standard is matched up with the transparent color - currently no 17 or 16 ?
    #harbourcols <- cbind(sbs.amp.stndnm,col.01,sbs.amp$Harbour)
    #replace the colour for the standard dilution sample type with the transparent colour
    #col.02<-replace(col.01, col.01==16, transp_col)
    #col.04 <- colforharb$col.02[match(sbs.amp$Harbour.Welltype, colforharb$unHaWT)]
    
    
    ##  Draw the points over the grid lines.
    points( y.sbs.amp ~ x.sbs.amp, data = d.sbs.famp, 
            pch=c(24), lwd=1, cex=1.8,
            bg=as.character(sbs.amp$col.06)
    )
    #edit labels on the x-axis
    ticks <- seq(-1, 9, by=1)
    labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
    axis(1, at=c(0.1, 1, 10, 1e+2, 1e+3, 1e+4, 1e+5, 1e+6, 1e+7, 1e+8, 1e+9), pos=8, labels=labels)
    #edit labels on the y-axis
    axis(side=2, at=seq(8, 50, by = 2), las=1, pos=0.1)
    
    #estimate a model for each STD subset incl below LOQ
    sbs.amp$x <- sbs.amp$Quantitycopies
    sbs.amp$y<- sbs.amp$CtdRn
    # calculate the log10 for for the Quantitycopies
    sbs.amp$log10x <- log10(sbs.amp$Quantitycopies)
    #estimate a linear model 
    logEst.amp_STD <- lm(y~log(x),sbs.amp)
    # calculate the log10 for for the Quantitycopies
    sbs.amp$log10x <- log10(sbs.amp$Quantitycopies)
    #estimate a linear model 
    logEst.amp_STD <- lm(y~log(x),sbs.amp)
    #estimate a linear model for the log10 values
    # to get the slope
    log10xEst.amp_STD <- lm(y~log10x,sbs.amp)
    #add log regresion lines to the plot
    with(as.list(coef(logEst.amp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1))
    
    #estimate a model for each STD subset for dilution steps above LOQ
    ab.loq.sbs.amp<-zh # get the previously limited dataframe from identifying LOQ
    ab.loq.sbs.amp$x <- ab.loq.sbs.amp$Quantitycopies
    ab.loq.sbs.amp$y<- ab.loq.sbs.amp$CtdRn
    logEst.abloqamp_STD <- lm(y~log(x),ab.loq.sbs.amp) #make a linear model
    #get the slope to calculate the efficiency
    slo1 <- log10xEst.amp_STD$coefficients[2]
    slo2 <- as.numeric(as.character(slo1))
    
    intc1 <- log10xEst.amp_STD$coefficients[1]
    intc2 <- as.numeric(as.character(intc1))
    # If log(x) = -1.045
    #Then x = 10^-1.045 = 0.09015711
    #slo3 = 10^slo2
    #
    #Effic <- (-1/slo2)*100
    
    #Try with perfect efficiency
    #2^3.3219400300021
    #slo2 = -3.3219400300021
    #https://www.gene-quantification.de/efficiency.html
    #qPCR efficiency
    Effic <- (-1+(10^(-1/slo2)))*100
    #amplification factor
    ampF <- 10^(-1/slo2)
    rEffic <- round(Effic,2)
    intc3 <- round(intc2,2)
    slo3 <- round(slo2,2)
    #add log regresion lines to the plot
    with(as.list(coef(logEst.abloqamp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1, col="red"))
    
    #add 95% confidence intervals around each fitted line
    #inspired from this webpage
    #https://stat.ethz.ch/pipermail/r-help/2007-November/146285.html
    
    #for the first line - with below LOQ
    newx<-seq(lod.val,1e+6,1000)
    prdlogEst.amp_STD<-predict(logEst.amp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                               level = 0.95, scale=0.95 , type="response")
    prd2logEst.amp_STD<- prdlogEst.amp_STD
    #polygon(c(rev(newx), newx), c(rev(prd2[ ,3]), prd2[ ,2]), col = 'grey80', border = NA)
    lines(newx,prd2logEst.amp_STD[,2],col="black",lty=2)
    lines(newx,prd2logEst.amp_STD[,3],col="black",lty=2)
    
    
    #add 95% conf. intervals for the second line - only above LOQ
    newx<-seq(loq.val,1e+6,100)
    prdlogEst.abloqamp_STD<-predict(logEst.abloqamp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                                    level = 0.95, scale=0.95 , type="response")
    prd2logEst.abloqamp_STD<- prdlogEst.abloqamp_STD
    #polygon(c(rev(newx), newx), c(rev(prd2[ ,3]), prd2[ ,2]), col = 'grey80', border = NA)
    lines(newx,prd2logEst.abloqamp_STD[,2],col="red",lty=2)
    lines(newx,prd2logEst.abloqamp_STD[,3],col="red",lty=2)
    
    # add a legend for colors on points
    legend(1e+7*0.5,49,
           unique(sbs.amp$Harbour.Welltype),
           pch=c(24),
           bg="white",
           #NOTE!! the hex color numbers must be read as characters to translate into hex colors
           pt.bg = as.character(unique(sbs.amp$col.06)),
           y.intersp= 0.7, cex=0.9)
    
    # add a second legend for types of regression lines
    legend(1000,49,
           c("incl below LOQ","excl below LOQ"),
           #pch=c(24), #uncomment to get triangles on the line in the legend
           cex=0.8,
           bg="white",
           lty=c(1), col=c("black","red"),
           y.intersp= 0.8)
    
    # add a third legend for efficiency and R2
    legend(1e+7*0.5,28,
           c(paste("efficiency: ",rEffic," %",sep=""),
             paste("R2: ",rcor_sbs02,sep=""),
             paste("equation: y=",slo3,"log(x) +",intc3,sep="")),
           #pch=c(24), #uncomment to get triangles on the line in the legend
           cex=0.9,
           bg="white",
           #lty=c(1), col=c("black","red"),
           y.intersp= 1.0)
    
    #title(main=c(paste("qPCR standard curve - for ",spec.lat,"\n-(",sbs.dknm,")"),  sep = ""), 
    #        col.main="red",
    #    sub="My Sub-title", col.sub="blue",
    #    xlab="My X label", ylab="My Y label",
    #    col.lab="green", cex.lab=0.75)
    
    
    ########################################################
    # for loop on seasons end here
    ########################################################
    
  }    
  
  # add title for the pdf-page
  
  mtext(c(paste("suppmatr_10.04b_Appendix A",no.spc.app.plot,"."),  sep = ""), outer=TRUE, 
        #use at , adj and padj to adjust the positioning
        at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
        adj=3.4,
        padj=2,
        #use side to place it in te top
        side=3, cex=1.6, line=-1.15)
  #apply the par settings for the plot as defined above.
  par(op)
  # end pdf file to save as
  dev.off()  
  
  
  ########################################################
  # for loop on species end here
  ########################################################
}
#########################################################
################################################################################################

smpls03 <- smpls02

###############################################################################################
# start -calculate copies per L of filtered water
###############################################################################################

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
#make a variable with path and file
pth_and_fl <- paste(wd00,wd10,"/suppmatr_10.04c_MONIS4_eDNA_smpls02.1.csv", sep="")
#write out a csv-file
write.csv(smpls02.1, file = pth_and_fl)

###############################################################################################
# end -calculate copies per L of filtered water
###############################################################################################
