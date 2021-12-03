#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-


#################################################################################
#
# Code developed by Steen W. Knudsen at NIVA-DK

# The 20 non indeginuous marine species targeted for analysis are the same 20 
# species that the MONIS2, MONIS3 and MONIS4 project, carried out at NIVA-DK
#
#
# The overall purpose of the present code is to analyse the eDNA levels inferred
# in the qPCR setups performed over 2017-2018.


# This code contains the following sections:

# 4 - Plot of sampling locations on maps, for each species,
#   with indication of eDNA intensity for each location monitored, 
#   and with interpolation of eDNA levels between sampled locations


#################################################################################
#remove everything in the working environment, 
#without a warning!!
rm(list=ls())

wd00 <- "/home/hal9000/MONIS3_4_v1/"
# set working directory
wd10 <- "suppmatr10_plots_and_tables_from_Rcode"
wd09 <- "suppmatr09_Rcodes_for_analysing_qPCR_eDNA_data"
wd08 <- "suppmatr08_merged_qPCRdata_txt_reports"
#paste dirs together
wd <- paste0(wd00,wd09)
setwd(wd)
getwd()

#define path and file name for csv file to read in
#"suppmatr_10.05d_MONIS4_eDNA_smpls20.csv"
#"suppmatr106b_MONIS4_eDNA_smpls24.csv"
in.csv.file01 <- paste(wd00,wd10,"/suppmatr_10.05d_MONIS4_eDNA_smpls20.csv",sep="")
#read in csv file from previous code
MONIS4_df <-as.data.frame(read.csv(in.csv.file01,
                                 header = TRUE, sep = ",", quote = "\"",
                                 dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))

# define inputfile
in.csv.file02 <- paste(wd00,wd09,"/suppmatr_09.11_input_NISAR_spcs_tax_hierarchy01.csv",sep="")
#read in the collected and filtered and extracted water samples
tx_hierc_df <- 
  read.csv(in.csv.file02, sep = ",",
           stringsAsFactors = FALSE)


# define inputfile
in.csv.file03 <- paste(wd00,wd09,"/suppmatr_09.13_input_MONIS3_harbour_positions02.csv",sep="")
#read in the collected and filtered and extracted water samples
df_mv_barpos01 <- 
  read.csv(in.csv.file03, sep = ",",
           stringsAsFactors = FALSE)

#head(MONIS5eDNA09_df,5)


####################################################################################
# Start Appendix E
####################################################################################


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Use ipdw package to interpolate between marine sampling locations
# interpolate between sampling locations using coastlines as barriers
#-  as the fish swims, not as the crow flies!
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#code is prepared in 2019-Aug by Steen W. Knudsen
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#remove everything in the working environment, without a warning!!
#rm(list=ls())

#libr.path <- "/home/sknu003/uoa00029_runs/Rplot_tryout"
#.libPaths( c( libr.path, .libPaths()) )

#libr.path <- "/home/sknu003/uoa00029_runs/Rplot_tryout"
#libr.path <- "/scale_wlg_persistent/filesets/home/sknu003/R/x86_64-pc-linux-gnu-library/3.5"

#libr.path <- "/scale_wlg_persistent/filesets/home/sknu003/R/x86_64-pc-linux-gnu-library/3.6"
#.libPaths( c( .libPaths(), libr.path) )

#.libPaths()

#.libPaths( c( libr.path , .libPaths() ) )
#.libPaths()
#.libPaths(libr.path)
#.libPaths()
#chooseCRANmirror(graphics=FALSE)
#chooseCRANmirror(4)
#'chooseCRANmirror(graphics=FALSE, ind=4)'
#________________________________________________________________________________
#01 - use the two spatial dataframes as in this example    https://jsta.github.io/ipdw/articles/ipdw2.html

# also check out this website: https://globalfishingwatch.org/data-blog/working-with-our-downloadable-public-data-in-r/
# and this website: https://www.molecularecologist.com/2015/07/marmap/
#________________________________________________________________________________
# get the rgeos package
if(!require(rgeos)){
  install.packages("rgeos", repos='http://cran.us.r-project.org')
}
library(rgeos)

# get the ipdw package
if(!require(ipdw)){
  install.packages("ipdw", repos='http://cran.us.r-project.org')
}
library(ipdw)

# get the scales package
if (!requireNamespace("scales", quietly=TRUE))
  install.packages("scales", repos='http://cran.us.r-project.org')
library(scales)


# get the sf package
if (!requireNamespace("sf", quietly=TRUE))
  install.packages("sf", repos='http://cran.us.r-project.org')
library(sf)

# get the rnaturalearth package
if (!requireNamespace("rnaturalearth", quietly=TRUE))
  install.packages("rnaturalearth", repos='http://cran.us.r-project.org')
library(rnaturalearth)

#Read in the rgdal library
if (!requireNamespace("rgdal", quietly=TRUE))
  install.packages("rgdal", repos='http://cran.us.r-project.org')
library(rgdal)

# get the sp package
if (!requireNamespace("sp", quietly=TRUE))
  install.packages("sp", repos='http://cran.us.r-project.org')
library(sp)

#https://www.rdocumentation.org/packages/biogeo/versions/1.0/topics/dms2dd
# get biogeo package to be able to use 'dms2dd' function
if(!require(biogeo)){
  install.packages("biogeo", repos='http://cran.us.r-project.org')
}
library(biogeo)
#https://www.rdocumentation.org/packages/rgdal/versions/1.3-6/topics/readOGR

if(!require(spatstat)){
  install.packages("spatstat", repos='http://cran.us.r-project.org')
}
library(spatstat)

###########################################################################
# Set working directory and read in csv file

# set working directory to data folder
# setwd("pathToDirHere")
#setwd("/Users/steenknudsen/R_koder")
#setwd("/Users/steenknudsen/Documents/Documents/Post doc KU/MONIS4/R_koder_for_eDNA_analyse_MONIS4")
#getwd()


#?source
#https://stackoverflow.com/questions/31201561/running-r-commands-using-a-bash-script
#https://unix.stackexchange.com/questions/408355/running-r-script-via-shell-script-syntax-error-near-unexpected-token
#https://stackoverflow.com/questions/12048436/r-sourcing-files-using-a-relative-path
#https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

###########################################################################
# Use the moved lat lon positions for harbours instead of the
# originally plotted harbour positions. This is to make sure the
# interpolation between points as between points in water and not
# on land. If the original noted positions is prefered, then
# this part can be commented out

#get the latitude elements to turn them in to decimal degrees 
dd <- df_mv_barpos01$LatGr02
mm <- df_mv_barpos01$LatMin02
ss <- df_mv_barpos01$Latsek02
ns <- df_mv_barpos01$LatHemisph02
#append back to dataframe
df_mv_barpos01$lat_decp <- dms2dd(dd,mm,ss,ns)
#do the same for longitude positions
dd <- df_mv_barpos01$LonGr02
mm <- df_mv_barpos01$LonMin02
ss <- df_mv_barpos01$Lonsek02
ns <- df_mv_barpos01$LonHemisph02
#append back to dataframe
df_mv_barpos01$lon_decp <- dms2dd(dd,mm,ss,ns)
#match to df with eDNA levels
df_MONIS4$lat_decp02 <- df_mv_barpos01$lat_decp[match(df_MONIS4$Harbour, df_mv_barpos01$Harbour)]
df_MONIS4$lon_decp02 <- df_mv_barpos01$lon_decp[match(df_MONIS4$Harbour, df_mv_barpos01$Harbour)]
#Now replace the originally noted positions for harbours
# with the positions that most certainly are plotted in the sea
df_MONIS4$declat <- df_MONIS4$lat_decp02
df_MONIS4$declon <- df_MONIS4$lon_decp02


# Read the .csv file with collection localities
# the lon-lat 02 positions are positions close to the sampling site, but might not be
# positions in the 'sea' when the 'worldmap' in R is used. This might be a problem
# for positions in fiords and narrow straits. To be able to use these sampling locations
# an extra position further away in the sea is included. This is the lon-lat 03 positions.
# The idea is that these positions further away will enable the R-package 'ipdw' to
# interpolate between the sanpling locations, even though some of the sampling locations
# might be in narrow straits and fiords.

#make columns with year and month by splitting strings in column
MONIS4_df$year <- gsub("\\..*","",MONIS4_df$Coll_date)
MONIS4_df$day <- gsub("*.*\\.","",MONIS4_df$Coll_date)
MONIS4_df$month <- data.frame(do.call('rbind', strsplit(as.character(MONIS4_df$Coll_date),'.',fixed=TRUE)))[,2]
#MONIS4_df$month
#make the first letter a capital letter
MONIS4_df$month2 <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
     MONIS4_df$month,
     perl = TRUE)

#count the number of season to loop over
no.of.seasons <- length(unique(MONIS4_df$season))
# make a sequence of numbers to use in a data frame
no_for_season <- seq(1:no.of.seasons)
#get the names of the seasons -  to use in the loop below
categories.of.seasons <- sort(unique(MONIS4_df$season))
# make names for the seasons
names.of.seasons <- c("fall","spring")
# bind to a data frame
seaons_nms_df <- as.data.frame(cbind(no_for_season,categories.of.seasons,names.of.seasons))
# make one of the columns numeric
seaons_nms_df$no_for_season <- as.numeric(seaons_nms_df$no_for_season)

#make three letter code abbreviations for harbours
MONIS4_df$Harbour.abbr1 <- MONIS4_df$Harbour
MONIS4_df$Harbour.abbr1 <- gsub("Fredericia", "Frc", MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("Frederikshavn","Frh" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("Aalborgportland","Alp" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("AalborgHavn","Alh" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("KalundborgStatiolHavn","Kas" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("Kalundborg","Kab" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("Koebenhavn","Kob" , MONIS4_df$Harbour.abbr1)
MONIS4_df$Harbour.abbr1 <- gsub("Koege","Kog" , MONIS4_df$Harbour.abbr1)
#append back to data frame
MONIS4_df$Harbour.abbr1 <- substr(MONIS4_df$Harbour.abbr1,1,3)
#convert all three letters to capital letters
MONIS4_df$Harbour.abbr1 <- toupper(MONIS4_df$Harbour.abbr1)

#Copy the data frame
#MONIS4_df <- MONIS5eDNA09_df
#replace NAs with zeroes
MONIS4_df[is.na(MONIS4_df)] <- 0
#https://www.edureka.co/community/1278/how-to-convert-dataframe-columns-from-factors-to-characters
#Only transform the columns that are factors into columns that instead are characters
i <- sapply(MONIS4_df, is.factor)
MONIS4_df[i] <- lapply(MONIS4_df[i], as.character)
# get only columns with characters
MONIS4.4_df <- MONIS4_df[, sapply(MONIS4_df, class) == 'character']
# get only columns with numeric values
MONIS4.3_df <- MONIS4_df[, sapply(MONIS4_df, class) == 'numeric']
#check which columns are characters, and which columns are factors, and which are numeric
# sapply(MONIS4_df, class) == 'factor'
# sapply(MONIS4_df, class) == 'character'
# sapply(MONIS4_df, class) == 'numeric'
#get columns names for the data frames defined above
MONIS4_colnm_chr <- colnames(MONIS4.4_df)
MONIS4_colnm_num <- colnames(MONIS4.3_df)
#subset the data frame by columns names
MONIS4_colnm_chr.2 <- MONIS4_df[MONIS4_colnm_num]
#https://stackoverflow.com/questions/47418127/r-how-to-aggregate-some-columns-while-keeping-other-columns
#paste columns together
MONIS4_df$spc.year.EuroFinsampleno.season <- paste(MONIS4_df$spc,MONIS4_df$year,MONIS4_df$EuroFinsampleno,MONIS4_df$season,sep=".")
#aggregate on all columns - this will introduce NAs for charcater columns
MONIS4.5_df <- aggregate(MONIS4_df, by = list(MONIS4_df$spc.year.EuroFinsampleno.season), FUN = mean, na.rm=T)
#delete columns with only NAs #https://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas
#and place back into same df object
MONIS4.5_df <- MONIS4.5_df[, colSums(is.na(MONIS4.5_df)) != nrow(MONIS4.5_df)]
#aggregate on all columns that have characters, in case multiple options are available per group, then take the first
MONIS4.6_df <- aggregate(MONIS4_df[,c(MONIS4_colnm_chr)], by = list(MONIS4_df$spc.year.EuroFinsampleno.season), FUN = function(a) a[1])
##rename a specifically selected column
names(MONIS4.5_df)[names(MONIS4.5_df) == 'Group.1'] <- 'spc.year.smpltp_grp'
names(MONIS4.6_df)[names(MONIS4.6_df) == 'Group.1'] <- 'spc.year.smpltp_grp'
#merge the two dataframes based on the common column
MONIS4.7_df <- merge(
  MONIS4.5_df,
  MONIS4.6_df,
  by = "spc.year.smpltp_grp"
)
#check resulting data frame
#head(MONIS4.7_df,8)
MONIS4eDNA01_df <- MONIS4.7_df


###########################################################################
# Transform catch positions to decimal degrees
# Match the decimal degrees w the harbours and with the eDNA levels
#get the latitude elements to turn them in to decimal degrees
# dd <- harb03$LatGr02
# mm <- harb03$LatMin02
# ss <- harb03$Latsek02
# ns <- harb03$LatHemisph02
# #append back to dataframe
# harb03$lat_decp <- dms2dd(dd,mm,ss,ns)
# #do the same for longitude positions
# dd <- harb03$LonGr02
# mm <- harb03$LonMin02
# ss <- harb03$Lonsek02
# ns <- harb03$LonHemisph02
# #append back to dataframe use 'dms2dd' function from 'biogeo' package
# harb03$lon_decp <- dms2dd(dd,mm,ss,ns)
# #match with eDNA data frame
# loc_edna01$lat_decp02 <- harb03$lat_decp[match(loc_edna01$Harbour, harb03$Harbour)]
# loc_edna01$lon_decp02 <- harb03$lon_decp[match(loc_edna01$Harbour, harb03$Harbour)]
#
# #get the latitude elements to turn them in to decimal degrees
# dd <- harb03$LatGr03
# mm <- harb03$LatMin03
# ss <- harb03$Latsek03
# ns <- harb03$LatHemisph03
# #append back to dataframe
# harb03$lat_decp03 <- dms2dd(dd,mm,ss,ns)
# #do the same for longitude positions
# dd <- harb03$LonGr03
# mm <- harb03$LonMin03
# ss <- harb03$Lonsek03
# ns <- harb03$LonHemisph03
# #append back to dataframe use 'dms2dd' function from 'biogeo' package
# harb03$lon_decp03 <- dms2dd(dd,mm,ss,ns)
# #match with eDNA data frame
# loc_edna01$lat_decp03 <- harb03$lat_decp03[match(loc_edna01$Harbour, harb03$Harbour)]
# loc_edna01$lon_decp03 <- harb03$lon_decp03[match(loc_edna01$Harbour, harb03$Harbour)]

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Start - section 01 - use only eDNA above LOQ for ipdw
# interpolation
# edit in this section to make use of eDNA levels below
# LOQ

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Log-10 transform the eDNA levels
#take the log10 to the column with eDNA copy numbers
# loc_edna01$copy.per.L.log10 <- log10(loc_edna01$copy.per.L)

###########################################################################
# Get the highest levels of eDNA
#paste two columns together
MONIS4eDNA01_df$gen_specnm.year_inds <- paste(MONIS4eDNA01_df$spc,MONIS4eDNA01_df$year,sep=".")
#get max value per group - this will be needed for the ipdw plots, where you want to
#set a max value on the legend for the heatmap
#https://stackoverflow.com/questions/25314336/extract-the-maximum-value-within-each-group-in-a-dataframe
max_copy_L_log10_p_spc_df <- aggregate(MONIS4eDNA01_df$log.10_copies_L, by = list(MONIS4eDNA01_df$gen_specnm.year_inds), max)
colnames(max_copy_L_log10_p_spc_df) <- c("gen_specnm.year_inds","max.log.10_copies_L")
#match back to original dataframe
MONIS4eDNA01_df$max.log.10_copies_L <- max_copy_L_log10_p_spc_df$max.log.10_copies_L[match(MONIS4eDNA01_df$gen_specnm.year_inds, max_copy_L_log10_p_spc_df$gen_specnm.year_inds)]

#If the copy number is above LOQ then return the
#copy number per L water plus 1
#first evaluate whether the value is above LOQ
  MONIS4eDNA01_df$eval_eDabLQ<- (MONIS4eDNA01_df$meanQuantitycopies>MONIS4eDNA01_df$LOQ)*1
  #if 
  MONIS4eDNA01_df$cpLw <- MONIS4eDNA01_df$eval_eDabLQ*MONIS4eDNA01_df$copies_per_Lwater
  #replace any zeroes with 1
  #to
  #MONIS4eDNA01_df$cpLwp1[(MONIS4eDNA01_df$cpLwp1==0)] <- 1
  #Note the addition of 1 to be able to take the log10 
  #to the number
  #and take the logarithm to this value
  MONIS4eDNA01_df$cpLwp1l10<- log10(MONIS4eDNA01_df$cpLw+1)
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# End  - section 01 - use only eDNA above LOQ for ipdw
# interpolation
# edit in this section to make use of eDNA levels below
# LOQ

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  ###########################################################################
#try and get the coastline that is downloaded in a supporting
# directory
coastline10 <- ne_load(scale = 10, type = 'land', category = 'physical', destdir = paste(wd00,wd09,sep=""))
#check if the object with the coastline does not exists
if (!exists("coastline10"))
{
  #get the coastline
  # a scale close to '10' gives a fine detailed coast, but takes longer to calculate
  # a scale above to '60' gives a non-detailed coast, but is faster to calculate
  #coastline110 <- ne_download(scale = 110, type = 'land', category = 'physical')
  #coastline10 <- ne_download(scale = 110, type = 'land', category = 'physical')
  #coastline10 <- ne_load(scale = 10, type = 'land', category = 'physical', destdir = paste(wd00,wd09,sep=""))
  coastline10 <- ne_download(scale = 10, type = 'land', category = 'physical')
  #coastline10 <- ne_download(scale = 40, type = 'land', category = 'physical')
  
  #close the if test above - i.e. if the 'coastline10' object does not exist, then get it
}
###########################################################################

# Assigning CRS
#Note the CRS is different from the UTM CRS prepared above for this CRS based on lonlat
r2 <- CRS("+init=epsg:4326") # the 4326 works for Northern Europe
# create crs object
epsg4326nCRS2 <- crs(r2)

##########################################################################################
#make a new column in the data frame with colors for taxonomical levels
tx_hierc_df$genus_spec <- paste(tx_hierc_df$Genus,tx_hierc_df$species,sep="_")
##########################################################################################
##########################################################################################
# section to try out different factors to adjust the resolution and to try out
# different factors to adjust the mean  neighbouring distance
# start --
##########################################################################################
#r_mnd <- seq(0.4,2, 0.1)
nr_mnd <- seq(0.2,2, 0.1)
res_m <- seq(0.01,0.5, 0.04)
res_m <- 0.01
res_m <- 0.06

nr_mnd <- 1.8
r_mnd <- 1.8
#mean neighbouring distance of 0.9 allows interpolation for
#Prorocentrum minimum
nr_mnd <- 0.9
r_mnd <- 0.9
#res_fac <- 0.25
#res_m <- 0.25
##########################################################################################
# section to try out different factors to adjust the resolution and to try out
# different factors to adjust the mean  neighbouring distance
# end --
##########################################################################################

##########################################################################################
# based on the section above trying out different factors to adjust
# the resolution and to try out the mean  neighbouring distance - use this
# data frame for the species
# start --
##########################################################################################
unq.spc.years <- unique(MONIS4eDNA01_df$gen_specnm.year_inds)

#unq.spc.years <- "Mnemiopsis leidyi.2018"
# col.h2 <- c("spc.lat.nm","mean.neigh.dist","res_fact")
# Mnelei <- c("Mnemiopsis_leidyi", 1.2, 0.25) # coarse resolution = faster to calculate
# #Mnelei <- c("Mnemiopsis_leidyi", 1.2, 0.05) # fine resolution = slower to calculate
# Myaare <- c("Mya_arenaria",0.4, 0.25) # coarse resolution = faster to calculate
# #Myaare <- c("Mya_arenaria",0.4, 0.03) # fine resolution = slower to calculate
# Colper <- c("Colpomenia_peregrine",0.5,0.25) # coarse resolution = faster to calculate
# #Colper <- c("Colpomenia_peregrine",0.01,0.25) # fine resolution = slower to calculate
# Psever <- c("Pseudochattonella_verruculosa",1.1,0.25)
# Psefar <- c("Pseudochattonella_farcimen",0.5,0.25) # coarse resolution = faster to calculate
# Karmik <- c("Karenia_mikimotoi",0.35,0.25)
# Bonham <- c("Bonnemaisonia_hamifera", 0.4, 0.25)
# Promin <- c("Prorocentrum_minimum", 0.8,0.25)
# Cragig <- c("Crassostrea_gigas",0.4,0.1)
#
#
# col.h2 <- c("spc.lat.nm","res_fact","mean.neigh.dist")
# Acibae <- c("Acipenser_baerii",0.05,0.5)
# Bonham <- c("Bonnemaisonia_hamifera",0.05,0.5)
# Caraur <- c("Carassius_auratus",0.05,0.5)
# Colper <- c("Colpomenia_peregrine",0.01,0.5)
# Corcas <- c("Cordylophora_caspia",0.05,0.5)
# Cragig <- c("Crassostrea_gigas",0.05,0.5)
# Cypcar <- c("Cyprinus_carpio",0.05,0.5)
# Erisin <- c("Eriocheir_sinensis",0.05,0.5)
# Homame <- c("Homarus_americanus",0.05,0.5)
# Karmik <- c("Karenia_mikimotoi",0.05,0.5)
# Mnelei <- c("Mnemiopsis_leidyi",0.05,0.5)
# Myaare <- c("Mya_arenaria",0.01,0.5)
# Neomel <- c("Neogobius_melanostomus",0.05,0.5)
# Oncmyk <- c("Oncorhynchus_mykiss",0.05,0.5)
# Oncgor <- c("Oncorhyncus_gorbuscha",0.05,0.5)
# Parcam <- c("Paralithodes_camtschaticus",0.05,0.5)
# Promin <- c("Prorocentrum_minimum",0.09,0.5)
# Psefar <- c("Pseudochattonella_farcimen",0.05,0.5)
# Psever <- c("Pseudochattonella_verruculosa",0.05,0.5)
# Rhihar <- c("Rhithropanopeus_harrisii",0.05,0.5)
#
# #fc.spc_df <- as.data.frame(rbind(Mnelei, Myaare, Colper, Psever, Psefar, Karmik, Bonham, Promin, Cragig))
# fc.spc_df <- as.data.frame(rbind(Acibae, Bonham, Caraur, Colper, Corcas, Cragig, Cypcar, Erisin, Homame, Karmik, Mnelei, Myaare, Neomel, Oncmyk, Oncgor, Parcam, Promin, Psefar, Psever, Rhihar))
# colnames(fc.spc_df) <- col.h2
# #get first two rows of df
# #fc.spc_df <- fc.spc_df[1:2,]

##########################################################################################
# based on the section above trying out different factors to adjust
# the resolution and to try out the mean  neighbouring distance - use this
# data frame for the species
# end --
##########################################################################################


##########################################################################################
# use this loop below to try out different factors to adjust
# the resolution and to try out the mean  neighbouring distance
# start --
##########################################################################################

#for (r_mnd in nr_mnd) { #loop for mean neighbouring distance
for (res_fac in res_m) { #loop for resolution in costras
  print(paste("mnd:",r_mnd))
}

##########################################################################################
# use this loop below to try out different factors to adjust
# the resolution and to try out the mean  neighbouring distance
# end -- NOTICE that the curly bracket for this loop is ending further down !!
##########################################################################################

#################################################################################
# BEGIN LOOP over species for making 2 ipdw maps , one map per season
#################################################################################
#Extract Unique Elements from main data frame
#unq.spc.seas <- unique(loc_edna01$spc.season)
unq.spc <- unique(MONIS4eDNA01_df$spc)
#replace underscore with space
unq.spc <- gsub("_"," ",unq.spc)
#uncomment below if you want to test the iteration
# with a single species
#unq.spc <- "Colpomenia_peregrine"
#unq.spc <- "Mnemiopsis leidyi"
#unq.spc <- "Pseudochattonella farcimen"
#unq.spc <- "Mya arenaria"
#unq.spc <- "Bonnemaisonia hamifera"
#unq.spc <- "Bonnemaisonia hamifera"
#unq.spc <- "Crassostrea gigas"
#unq.spc <- "Pseudochattonella verruculosa"
#unq.spc <- "Prorocentrum minimum"
#Extract Unique Elements from shortened dataframe
yrs <- unique(as.numeric(MONIS4eDNA01_df$year))
#yrs <- "2018"
#count the elements
no.y2 <- length(yrs)
# make sequence of numbers
no.e1 <- seq(1:no.y2)
#bind columns and make a data frame
no.e2 <- as.data.frame(cbind(no.e1, yrs))
#get maximum number of years
nppy <- max(no.e1)


# -loop over species
for (spec.lat in unq.spc) {
  print(spec.lat)
  #}
  
  #get the latin species name without underscore
  spec.lat.w_undersc <- paste(sub(' ', '_', spec.lat))
  spec.lat.no_undersc <- spec.lat
  #subset the dataframe based on variable value in column
  MONIS4eDNA02_df <- MONIS4eDNA01_df[which(MONIS4eDNA01_df$spc == spec.lat.w_undersc),]
  
  #get the Danish commom name
  #first split the string by the dot
  #https://stackoverflow.com/questions/33683862/first-entry-from-string-split
  #and escape the dot w two backslashes
  latnm <- sapply(strsplit(spec.lat, "\\."), `[`, 1)
  #Get DK common name
  sbs.dknm <- unique(MONIS4eDNA02_df$dk_comnm)
  #get AssIDNo
  sbs.AssIDNo <- unique(MONIS4eDNA02_df$AssayIDcode)
  #get the number for the appendix plot number
  no.spc.app.plot <- gsub("AssID","",sbs.AssIDNo)
  #no.spc.app.plot <- unique(MONIS4eDNA02_df$AssayID)
  
  #get the latin species name with an underscore
  spec.lat <- sub(' ', '_', spec.lat)
  #use two functions together 
  genusl <- substr(spec.lat.w_undersc, 1, 1)
  spcl <- gsub("*.*_","",spec.lat.w_undersc)
  #and paste them together
  # to get an abbreviated species name
  short.spec.lat <- paste(genusl,"_",spcl,sep="")
  
  # use the data frame with best inferred resolutions and multiplier for
  # mean neighbouring distance
  # These factors were inferred using sequences of different multipliers
  # to see which returned a detailed heat map for each of the species
  #match species name to get resolution factor
  #res_fac <- fc.spc_df$res_fact[match(spec.lat, fc.spc_df$spc.lat.nm)]
  #res_fac<-as.numeric(as.character(res_fac))
  #match species name to get  factor for multiplying mean neighbouring distance
  #r_mnd <- fc.spc_df$mean.neigh.dist[match(spec.lat, fc.spc_df$spc.lat.nm)]
  #r_mnd<-as.numeric(as.character(r_mnd))
  # Exporting PFD files via postscript()
  #first make a file name
  flnm <- paste(
    wd00,wd10,"/suppmatr_10.07a_App_E_",sbs.AssIDNo,
    "_ipwd_res",res_fac,"_mnd",
    r_mnd,
    "_",
    short.spec.lat,
    ".pdf",
    sep = ""
  )
  #use the filename in the plot to print to pdf
  pdf(file=flnm,
      #define the dimensions on the page in the pdf file
      width = (2*1.6 *1* 8.2677),
      height = (nppy*1.6 * 2.2 * 2.9232))
  # if this throws an error about not being able to open the pdf it is most
  # likely because you have the Adobe Acrobat reader open already.
  # It cannot write the file to a file you already have open in another application
  # Close the Adobe Acrobat reader, and try again
  
  #set plotting margins
  op <-
    par(
      mfrow = c(nppy, 2), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots - "c('plots in rows', 'plots in columns')"
      oma = c(1, 1, 0, 0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
      mar = c(5, 5, 3, 5) # set the margin around each individual plot . with sides ordered as: " c(bottom,left,top,right)"
    )
  
  # to test the loop with one variable  use a single selected year
  #yrs <- "2017"
  #yr_smpl <- "2017"
  
  #loop over years sampled -  to produce individual tables per year sampled
  for (yr_smpl in yrs){
    print(yr_smpl)
    #}
    
    #get maximum eDNA log 10 level
    #get prefered upper limit of legend, rounded up - use later for zlim
    zvm<-ceiling(max(MONIS4eDNA02_df$cpLwp1l10))
    
    #subset the original dataframe per year sampled
    #to be able to plot eDNA evaluation squares
    MONIS4eDNA03_df <-
      MONIS4eDNA02_df[which(MONIS4eDNA02_df$year == yr_smpl),]
    # to test the loop with one variable  use a single selected season
    # season <- "season_2"
    #categories.of.seasons <-"season_1"
    #reverse the order of the elements in the vector
    # because you will want to have the plot with the ipdw
    #map for spring on the left, and the fall ipdw map on the right
    rctse <- rev(categories.of.seasons)
    #iterate over the season in the vector
    for (season in rctse){
      print(season)
      #}
      
      
      #use match to match the season with a data frame and get the name for the season
      spcfc_seaon_name <- seaons_nms_df$names.of.seasons[match(season, seaons_nms_df$categories.of.seasons)]
      spcfc_seaon_name <- as.character(spcfc_seaon_name)
      
      # subset among the months to get from 1 to 6
      sbs.pery.MONIS4eDNA03_season_df <- MONIS4eDNA03_df[ which(MONIS4eDNA03_df$season== season), ]
      #check if the data frame is empty . See this question : https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
      #check across two statements
      if (dim(sbs.pery.MONIS4eDNA03_season_df)[1] == 0) {
        print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
        #}
        #if subsetted data fram for spring is empty - no samples
        #then create an empty data frame with zeroes and no color for points
        #c_M27_season_no <- length(sbs.pery.MONIS4eDNA03_season_df)
        #sbs.pery.MONIS4eDNA03_season2_df <- rbind(sbs.pery.MONIS4eDNA03_season_df,(rep(0,c_M27_season_no)))
        #colnames(sbs.pery.MONIS4eDNA03_season2_df) <- colnames(sbs.pery.MONIS4eDNA03_season_df)
        #sbs.pery.MONIS4eDNA03_season_df <- sbs.pery.MONIS4eDNA03_season2_df
        #sbs.pery.MONIS4eDNA03_season_df$eDNA_eval_t_repl_col <- "#000000FF"
        #
        plot.new()
      }
      # check for multiple situations - second check if the data frame has more than one dimension - https://www.datamentor.io/r-programming/if-else-statement/
      # if the dataframe does have more than one dimension, then try and plot it
      else if (dim(sbs.pery.MONIS4eDNA03_season_df)[1] >= 1)
        #start curly bracket for 'else if' test testing whether the dimensions on the data frame is more than one, if it is then make the plot on the map
      {
        
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lon <- min(sbs.pery.MONIS4eDNA03_season_df$declon)
        M27_jmax_lon <- max(sbs.pery.MONIS4eDNA03_season_df$declon)
        jit_lon <- (M27_jmax_lon-M27_jmin_lon)/80000
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lat <- min(sbs.pery.MONIS4eDNA03_season_df$declat)
        M27_jmax_lat <- max(sbs.pery.MONIS4eDNA03_season_df$declat)
        jit_lat <- (M27_jmax_lat-M27_jmin_lat)/80000
        #jitter points to work around overlapping points
        sbs.pery.MONIS4eDNA03_season_df$jit.lok_pos_lon <- jitter(sbs.pery.MONIS4eDNA03_season_df$declon, jit_lon)
        sbs.pery.MONIS4eDNA03_season_df$jit.lok_pos_lat <- jitter(sbs.pery.MONIS4eDNA03_season_df$declat, jit_lat)
        #make SpatialPointsDataFrame with decimal degree coordinates
        #make points with decimal coordinates - this makes use of a different CRS
        pnts2 <- SpatialPointsDataFrame(sbs.pery.MONIS4eDNA03_season_df[,c("jit.lok_pos_lon","jit.lok_pos_lat")],
                                        sbs.pery.MONIS4eDNA03_season_df,
                                        proj4string = crs(epsg4326nCRS2))
        #make another data frame
        pnts3 <- SpatialPointsDataFrame(sbs.pery.MONIS4eDNA03_season_df[,c("declon","declat")],
                                        sbs.pery.MONIS4eDNA03_season_df,
                                        proj4string = crs(epsg4326nCRS2))
        #pnts_test <- as.data.frame(pnts2)
        #make points in decimal degr coordinates in a spatial dataframe for sampl locations
        #but moved a bit aside to plot eDNA evaluations - for the season
        sbs.pery.MONIS4eDNA03_season_df$lok_pos_lon.f.pnts4 <- sbs.pery.MONIS4eDNA03_season_df$jit.lok_pos_lon + 0
        sbs.pery.MONIS4eDNA03_season_df$lok_pos_lat.f.pnts4 <- sbs.pery.MONIS4eDNA03_season_df$jit.lok_pos_lat + 0.16
        
        
        #turn these new points in to a spatial data frame
        pnts4 <-
          SpatialPointsDataFrame(sbs.pery.MONIS4eDNA03_season_df[, 
                                  c("lok_pos_lon.f.pnts4", "lok_pos_lat.f.pnts4")],
                                 sbs.pery.MONIS4eDNA03_season_df,
                                 proj4string = crs(epsg4326nCRS2))
        
        #prepare bounding box for decimal degrees
        bbox_k2 <- raster::buffer(
          as(extent(spTransform(pnts2, projection(coastline10))), 
             "SpatialPolygons"),
          width = 7) # A width=10 zooms up and includes more sourrounding landmass  in the map. A width=2 zooms in and includes less landmass around in the map.
        #project coastline on bbbox
        projection(bbox_k2) <- projection(coastline10)
        pols2 <- raster::crop(coastline10, bbox_k2)
        #transform to spdf with decim degr coordinates
        pols2 <- spTransform(pols2, projection(pnts2))
        
        #make costras with lon lat  coordinates
        costras2 <- costrasterGen(pnts2, pols2, extent = "polys",
                                  projstr = projection(pols2),
                                  resolution = res_fac)
        
        #resolution = 0.08) #Note this is in decimals for lonlat 
        #coordinates. Higher value (>1) makes interpolation goes fast, 
        #low (<0.05) takes too much RAM and too much time
        #make ipdw result 'res.ipdw2' based on pnts with lon-lat 
        #coordinates
        #This ipdw result 'res.ipdw2' is different in blending between 
        #colours between points
        #As compared to the section below that involves the 'training' 
        #element in 'res.ipdw3'
        # res.ipdw2 <- ipdw::ipdw(pnts2, costras2, paramlist = "copy.per.L.log10",
        #                         range = 10, #low range reduces areas around point. #Note this is in decimals for lonlat coordinates.
        #                         dist_power = 4)
        #increasing the 'dist_power' to 10 makes gradients between points less pronounced, and instead colors more intensely up to the borders
        
        ###################################################################################
        pnts <- pnts2
        costras <- costras2
        # find average nearest neighbor
        library(spatstat)
        
        W              <- owin(range(coordinates(pnts)[,1]),
                               range(coordinates(pnts)[,2]))
        kat.pp         <- ppp(coordinates(pnts)[,1], 
                              coordinates(pnts)[,2], window = W)
        #if the ppp function complains about duplicated points, you can check which ones are duplicated
        #duplicated(kat.pp)
        #go back and check your original lon-lat columns in the input data frame and make sure no lon-lat are duplicated.
        mean.neighdist <- mean(nndist(kat.pp))
        # grid building
        gridsize       <- mean.neighdist * 1*r_mnd #increasing the multiplier makes the gradients blend more across the borders equidistant from each point
        r_mnd1.lonlat       <- (mean.neighdist * 1*r_mnd)
        grainscale.fac <- gridsize / res(costras)[1]
        gridras        <- aggregate(costras, fact = grainscale.fac)
        #as.data.frame(costras)
        gridpol        <- rasterToPolygons(gridras)
        gridpol$value  <- row.names(gridpol)
        #check pnts as a dataframe
        #df.pnts01  <- data.frame(pnts)
        #colnames(df.pnts01)
        # spatial join
        fulldataset.over    <- over(pnts, gridpol)
        fulldataset.over    <- cbind(data.frame(fulldataset.over),
                                     setNames(data.frame(pnts),
                                              c(colnames(data.frame(pnts)))))
        #colnames(data.frame(pnts))
        #fulldataset.over
        # grid selection
        set.seed(2)
        gridlev <- unique(fulldataset.over$value)
        for(i in seq_along(gridlev)){
          activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
          selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
          if(i == 1){
            training <- activesub[selectnum,]
          }
          else{
            training <- rbind(training, activesub[selectnum,])
          }
        }
        #####
        validate             <- fulldataset.over[!(row.names(fulldataset.over) %in%
                                                     row.names(training)),]
        #Make sure you change the 'lon_decp02' and 'lat_decp02' to your own lon-lat columns in your original dataframe
        #Make sure you change the 'jit.lok_pos_lon' and 'jit.lok_pos_lat' to your own lon-lat columns in your original dataframe
        xy                   <- cbind(training$jit.lok_pos_lon, training$jit.lok_pos_lat)
        training             <- SpatialPointsDataFrame(xy, training)
        xy                   <- cbind(validate$jit.lok_pos_lon, validate$jit.lok_pos_lat)
        validate             <- SpatialPointsDataFrame(xy, validate)
        projection(training) <- projection(pnts)
        projection(validate) <- projection(pnts)
        ####
        
        #paramlist is the z-value to interpolate across
        # use "log.10_copies_L" as input parameter
        # to use eDNA lvls that at least are above zero 
        pl <- c("log.10_copies_L")
        # use the "cpLwp1l10" to only use eDNA lvls 
        # above LOQ
        pl <- c("cpLwp1l10") # use the copy number per L plus 1
        
        #but only for above LOQ
        
        #make the ipdw raster
        res.ipdw3 <- ipdw(training, costras, range = mean.neighdist * 8*r_mnd, pl,
                          overlapped = TRUE, dist_power = 1.0)
        r_mnd2.lonlat <- (mean.neighdist * 8 *r_mnd)
        #low range reduces areas around point. #Note this is in decimals for lonlat coordinates.
        #increasing the 'dist_power' to 10 makes gradients between points less pronounced, and instead colors more intensely up to the borders
        
        
        #get color from the subsetted data frame
        col.f.ramp.pal <- "blue" # unique(loc_edna04$col_f_Phyl)
        col.f.ramp.pal <- "orange" #unique(loc_edna04$spcf_col_f_Phyl)
        #match species name in loop with the color for the species
        phy_col <- tx_hierc_df$col_f_Phyl[match(spec.lat,tx_hierc_df$genus_spec)]
        #okace the color in the object that is needed in the plot
        col.f.ramp.pal <- phy_col
        #col.f.ramp.pal <- "darkgreen" #unique(loc_edna04$spcf_col_f_Phyl)
        #make a colour range
        colfunc03 <-
          colorRampPalette(c("white", col.f.ramp.pal, "black"))
        Apts = 30
        #make color range
        cols01 <- colfunc03(30) #white to phylum-color to black
        #get prefered upper limit of legend, rounded up - use later for zlim
        #zvm<-ceiling(max(loc_edna04$copy.per.L.log10))
        #define limits to the axis of the plot
        xx <- labeling::extended(3, 17, 14, only.loose=TRUE)
        yy <- labeling::extended(54, 60,6, only.loose=TRUE)
        zz <- labeling::extended(0, zvm,zvm, only.loose=TRUE)
        #define limits to use to crop map
        e <- extent(3, 17, 54, 59.5)
        #crop the map before plotting
        res.ipdw3c <- crop(res.ipdw3, e)
        
        #plot heatmap 02 - with decimal degress and lon lat
        plot(res.ipdw3c, #main = "copy.per.L.log10 lonlat train",
             col=cols01, #set color for z-value
             zlim=c(0,zvm), #limit the z-value in the legend and map
             xlim=c(3,17),
             ylim=c(54,60),
             xaxt="n", #do not use the default tick labels on the x-axis
             yaxt="n", #do not use the default tick labels on the y-axis
             #xlab = "Longitude", #label along x-axis
             #ylab = "Latitude", #label along y-axis
             las=1,  #las=1 turns the tick label on the axis
             cex.axis=2) #adjust size
        #Add text along x- and y-axis in specified color and size
        mtext("longitude", side=1, line=3, col="black", cex=2)
        mtext("latitude", side=2, line=3, col="black", cex=2)
        #add land
        plot(pols2, add = TRUE, col="black", bg="azure4")
        #add land in a different colour
        plot(pols2, add = TRUE, col="azure4")
        
        #add the the eDNA evaluations for sampling points as symbols coloured by column
        plot(pnts4,add = TRUE,pch = 22,col = "black",bg = c(as.data.frame(pnts4)$eDNA_eval_t_repl_col),cex = 3.0)
        # add sampling locations coloured by conventional monitoring result
        plot(pnts3,add = TRUE,pch = 25,col = "blue",bg = c(as.data.frame(pnts3)$col.f.conv_rec_val),cex = 2.6)
        #add special ticks and adjust the size of the text associated 
        #with these tick marks
        axis(1, at = xx, cex.axis=2.0)
        axis(2, at = yy, las=1, cex.axis=2.0) #las=1 turns the labels at the tick marks on the axis
        #set range for z key
        zz <- seq(0,zvm)
        #axis(5, at = zz, las=1, cex.axis=2.0) #las=1 turns the labels at the tick marks on the axis
        #deduct a bit from the latitude, to lower the positioning of 
        #the label
        redlatpt1 <- (max(as.data.frame(pnts4)$declat)-min(as.data.frame(pnts4)$declat))/350.6
        #add text to the same lon lat position for harbour, or for month

        # text(as.data.frame(pnts4)$declon,
        #      as.data.frame(pnts4)$declat-redlatpt1,
        #      as.data.frame(pnts4)$Harbour.abbr1,
        #      #as.data.frame(pnts4)$Harbour,
        #      #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
        #      pos=4,
        #      cex= 1.8
        # )
        # 
        #add a title for key-legend on side
        mtext("log10(eDNA copies/L)", side=4, line=5, cex=1.8)
        
        spec.lat.no_undersc <- paste(sub('_', ' ', latnm))
        #add a title with bquote
        #use 'atop' function to get the title to appear on two lines
        #see this website: https://stackoverflow.com/questions/20549337/expression-and-new-line-in-plot-labels?lq=1
        title(main = c(bquote(
          atop(
            'eDNA lvls-log10 from'
            ~ italic(.(spec.lat.no_undersc)),
            ~ '('
            ~ .(sbs.dknm)
            ~ '), Assay Id No:' ~ .(sbs.AssIDNo)
            ~ ' for '
            ~ .(spcfc_seaon_name)
            ~ ' in '
            ~ .(yr_smpl)
            ~ '  ')
        )))
        ##add legend for conventional monitoring  evaluation
        legend(
          "bottomleft",
          "(x,y)",
          bg = "white",
          c(
            "Unknown, no previous record",
            "Recorded in the past",
            "Recorded in 2017"
          ),
          ncol = 1,
          pch = c(25, 25, 25),
          pt.cex=2.2, #size of points in legend
          #set type of point
          #col= c("black", "black", "black"), #set color of point
          col = c("blue", "blue", "blue"),
          #set color of point
          #pt.bg=c(alpha(c("white", "red", "black"), 0.6)), #set background color of point
          pt.bg = c(c("white", "red", "black")),
          #set background color of point
          pt.lwd = c(1.0),
          title = "conventional monitoring",
          cex = 1.4,
          inset = 0.02
        )
        
        # # add legend for spring and fall
        # legend(
        #   "topleft",
        #   "(x,y)",
        #   bg = "white",
        #   c("spring", "autumn"),
        #   ncol = 1,
        #   pch = c(22, 22),
        #   #set type of point
        #   col = c("blue", "red"),
        #   #set color of point
        #   #pt.bg=c(alpha(c("white", "white"), 0.6)), #set background color of point
        #   pt.bg = c(c("white", "white")),
        #   #set background color of point
        #   pt.lwd = c(1.2),
        #   title = "season ",
        #   cex = 1.1,
        #   inset = 0.02
        # )
        
        # # add legend for eDNA evaluation
        legend(
          "topright",
          "(x,y)",
          bg = "white",
          c(
            "No Ct",
            "below LOD",
            "above LOD and below LOQ" ,
            "1 above LOQ",
            "3 above LOQ"
          ),
          ncol = 1,
          pch = c(22, 22, 22, 22, 22),
          pt.cex=2.4,
          #set type of point
          col = c("black", "black", "black", "black", "black"),
          #set color of point
          #pt.bg=c(alpha(c("white", "yellow", "black"), 0.6)), #set background color of point
          pt.bg = c(c(
            "white", "yellow", "orange", "red", "black"
          )),
          #set background color of point
          pt.lwd = c(1.0),
          title = "eDNA evalution ",
          cex = 1.4,
          inset = 0.02
        )
        
        
        #end curly bracket for 'else if' test testing whether the dimensions on the data frame is more than one, if it is then make the plot on the map
      }
      
      
      #end loop over seasons
    }
    #end loop over year
  }
  
  
  
  # add title for the pdf-page
  mtext(
    c(paste("Appendix E", no.spc.app.plot, "."),  sep = ""),
    outer = TRUE,
    #use at , adj and padj to adjust the positioning
    at = (par("usr")[1] + 0.15 * diff(par("usr")[1:2])),
    adj = 3.4,
    padj = 2,
    #use side to place it in te top
    side = 3,
    cex = 1.6,
    line = -1.15)
  #apply the par settings for the plot as defined above.
  par(op)
  # end pdf file to save as
  dev.off()
  
  ##end loop over species
}
##

#################################################################################
# END LOOP over species for making 2 ipdw maps , one map per season
#################################################################################

# use this end of loop to loop over factors adjusting the resolution in the costras
# and use the same loop to try out different factors adjusting the mean neighbouring distance
#}






#################################################################################


####################################################################################
# End Appendix E
####################################################################################


dev.off()

#define output file and path
outfile01 <- paste(wd00,wd10,"/suppmatr_10.07c_MONIS4eDNA01_df.csv",sep="")
#write the table to a csv
#to be used for the next R-code that plots eDNA levels on maps
write.csv(MONIS4eDNA01_df, file = outfile01)

#MONIS4eDNA01_df
