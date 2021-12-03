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




#################################################################################
#remove everything in the working environment, 
#without a warning!!
#rm(list=ls())

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
in.csv.file01 <- paste(wd00,wd10,"/suppmatr_10.07c_MONIS4eDNA01_df.csv",sep="")
#read in csv file from previous code
df_MONIS4 <-as.data.frame(read.csv(in.csv.file01,
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
 df_MONIS4$declon<- df_MONIS4$lon_decp02

###########################################################################
# Make a data frame with season categories

#count the number of season to loop over
no.of.seasons <- length(unique(df_MONIS4$season))
# make a sequence of numbers to use in a data frame
no_for_season <- seq(1:no.of.seasons)
#get the names of the seasons -  to use in the loop below
categories.of.seasons <- sort(unique(df_MONIS4$season))
# make names for the seasons
names.of.seasons <- c("fall","spring")
# bind to a data frame
seaons_nms_df <- as.data.frame(cbind(no_for_season,categories.of.seasons,names.of.seasons))
# make one of the columns numeric
seaons_nms_df$no_for_season <- as.numeric(seaons_nms_df$no_for_season)


#make three letter code abbreviations for harbours
df_MONIS4$Harbour.abbr1 <- df_MONIS4$Harbour
df_MONIS4$Harbour.abbr1 <- gsub("Fredericia", "Frc", df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("Frederikshavn","Frh" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("Aalborgportland","Alp" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("AalborgHavn","Alh" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("KalundborgStatiolHavn","Kas" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("Kalundborg","Kab" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("Koebenhavn","Kob" , df_MONIS4$Harbour.abbr1)
df_MONIS4$Harbour.abbr1 <- gsub("Koege","Kog" , df_MONIS4$Harbour.abbr1)
#append back to data frame
df_MONIS4$Harbour.abbr1 <- substr(df_MONIS4$Harbour.abbr1,1,3)
#convert all three letters to capital letters
df_MONIS4$Harbour.abbr1 <- toupper(df_MONIS4$Harbour.abbr1)

###########################################################################

#to convert lat and long to UTM coordinates see this website:
#https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
#this function assumes a WGS84 projection
#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#put the decimal degrees into x and y
#lon = longitude = values on x-axis
#lat = latitude = values on y-axis
# it took me more than a day to work out I had switched these around.
#A comment here: https://stackoverflow.com/questions/20054957/n-a-values-using-over-function-with-sp-package-in-r 
#made me suspect this was the reason for my code not working - the error did not show up before the 
#'over' function used below in this line : "fulldataset.over    <- over(pnts, gridpol)"
x<-df_MONIS4$declon
y<-df_MONIS4$declat

#put in a dataframe
#notice that the last variable is the utmzone number stored in the dataframe
utmzone <- 32
UTM_co <- LongLatToUTM(x,y,utmzone)

#append back to original data frame
#this assumes the order is unchanged
df_MONIS4$X_UTM <- UTM_co$X
df_MONIS4$Y_UTM <- UTM_co$Y

#add projection and utm zone - only for the example input file
#instead , for your own data you could have this information in additional columns
df_MONIS4$geodeticDa <- "WGS84"
df_MONIS4$utmZone <- 32
# look at the data structure
#str(df_MONIS4)
#class(df_MONIS4)

#define the two seasons to loop over
seasons <- c("foraar", "efteraar")
#seas <- "foraar"
#seasons <- "efteraar"
#seas <- "efteraar"
#make a dataframe for colors to match
seasons.cols01 <- c("foraar", "chartreuse","May-Jul")
seasons.cols02 <- c("efteraar", "chocolate1", "Sep-Nov")
seasons.col.lst <- t(data.frame(
  seasons.cols01,
  seasons.cols02
))
colnames(seasons.col.lst) <- c("season","color","Eng.month.rnge")
seasons.col.lst <- as.data.frame(seasons.col.lst)
seasons.col.lst$color <- as.character(seasons.col.lst$color)
seasons.col.lst


###########################################################################
#get the coastline from a previously downloaded file
coastline10 <- ne_load(scale = 10, type = 'land', category = 'physical', destdir = paste(wd00,wd09,sep=""))
#if the file with the coastline is unavailable to next 'if' check 
# will make sure that the coast line is downloaded
#check if the object with the coastline does not exists
if (!exists("coastline10"))
{
  #get the coastline
  # a scale close to '10' gives a fine detailed coast, but takes longer to calculate
  # a scale above to '60' gives a non-detailed coast, but is faster to calculate
  #coastline110 <- ne_download(scale = 110, type = 'land', category = 'physical')
  #coastline10 <- ne_download(scale = 110, type = 'land', category = 'physical')
  coastline10 <- ne_download(scale = 10, type = 'land', category = 'physical', destdir = paste(wd00,wd09,sep=""))
  coastline10 <- ne_load(scale = 10, type = 'land', category = 'physical', destdir = paste(wd00,wd09,sep=""))
  #coastline10 <- ne_download(scale = 40, type = 'land', category = 'physical')
  
  #close the if test above - i.e. if the 'coastline10' object does not exist, then get it
}
###########################################################################
##########################################################################################
#r_mnd <- seq(0.4,2, 0.1)
nr_mnd <- seq(0.2,2, 0.1)
res_m <- seq(0.01,0.5, 0.04)
res_m <- 0.06

nr_mnd <- 1.8
r_mnd <- 1.8*1e+6

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the number of species detected with eDNA lvls above zero -  start
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


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
#Extract Unique Elements from shortened dataframe
yrs <- unique(as.numeric(df_MONIS4$year))
#yrs <- "2018"
#count the elements
no.y2 <- length(yrs)
# make sequence of numbers
no.e1 <- seq(1:no.y2)
#bind columns and make a data frame
no.e2 <- as.data.frame(cbind(no.e1, yrs))
#get maximum number of years
nppy <- max(no.e1)

#paste columns together to be able to rearrange the 
#df by this column
df_MONIS4$Harbour.season <- 
  paste(df_MONIS4$Harbour,df_MONIS4$season,sep=".")
#make a column based on two criteria with '|' for 'OR'
# if eDNA detection is black or red, record the species 
#as being present
#This can be set to disregard the 'yellow' categories
# if a more strict criteria is desired
df_MONIS4$eDNA_presence1 <- 
  1*(df_MONIS4$eDNA_eval_t_repl_col=="black" | 
  df_MONIS4$eDNA_eval_t_repl_col=="red" |
    df_MONIS4$eDNA_eval_t_repl_col=="orange" |
    df_MONIS4$eDNA_eval_t_repl_col=="yellow" )

#count using the plyr-package - see: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
MONIS4_emnt01_tbl <- dplyr::count(df_MONIS4, season, Harbour, eDNA_presence1)
#make the tibble a dataframe
MONIS4_emnt02_df <- as.data.frame(MONIS4_emnt01_tbl)
#subset this dataframe to only include positive
# recorded monitorings - i.e. subset it to the 
# '1's for  eDNA_presence1'
MONIS4_emnt03_df <- subset (MONIS4_emnt02_df, eDNA_presence1 == 1)
# paste columns together, to be able to have a column to match 
# back to the large data frame
MONIS4_emnt03_df$Harbour.season <- 
  paste(MONIS4_emnt03_df$Harbour
        , MONIS4_emnt03_df$season, sep=".") 
# also make a column for whether the species
# is being recorded as being present by conventional 
# monitoring (cnvmnt), this setting includes the category:
# 'recorded in the past'
df_MONIS4$cnvmnt_presence1 <- (df_MONIS4$conv_rec_val>0)*1

#count using the plyr-package - see: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
MONIS4_cnvmnt01_tbl <- dplyr::count(df_MONIS4, season, Harbour, cnvmnt_presence1)
#make the tibble a dataframe
MONIS4_cnvmnt02_df <- as.data.frame(MONIS4_cnvmnt01_tbl)
#subset this dataframe to only include positive
# recorded monitorings - i.e. subset it to the 
# '1's for  cnvmnt_presence1'
MONIS4_cnvmnt03_df <- subset (MONIS4_cnvmnt02_df, cnvmnt_presence1 == 1)
# paste columns together, to be able to have a column to match 
# back to the large data frame
MONIS4_cnvmnt03_df$Harbour.season <- 
  paste(MONIS4_cnvmnt03_df$Harbour
        , MONIS4_cnvmnt03_df$season, sep=".") 
#now match back the count of positive recordings to the
# large data frame
#the new column counts up the no of species in harbour
# per season by conventional monitoring
df_MONIS4$nosiHp_cm <- MONIS4_cnvmnt03_df$n[match(df_MONIS4$Harbour.season,MONIS4_cnvmnt03_df$Harbour.season)]
#the new column counts up the no of species in harbour
# per season by eDNA monitoring
df_MONIS4$nosiHp_em <- MONIS4_emnt03_df$n[match(df_MONIS4$Harbour.season,MONIS4_emnt03_df$Harbour.season)]

# Assigning CRS
#Note the CRS is different from the UTM CRS prepared above for this CRS based on lonlat
r2 <- CRS("+init=epsg:4326") # the 4326 works for Northern Europe
# create crs object
epsg4326nCRS2 <- crs(r2)

######
  # Exporting PFD files via postscript()
filnm <- "no_spc_by_eDNAmnt"
  #first make a file name
  flnm <- paste(
    wd00,wd10,"/suppmatr_10.12a_App_F_",filnm,
    "_ipwd_res",res_fac,"_mnd",
    r_mnd,
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
    
    #get maximum recorded number of species by eDNA method
    #per harbopur
    #get prefered upper limit of legend, rounded up 
    # - use later for zlim
    zvm<-ceiling(max(df_MONIS4$nosiHp_em))
    
    #subset the original dataframe per year sampled
    #to be able to plot eDNA evaluation squares
    df_MONIS4eDNA03 <-
      df_MONIS4[which(df_MONIS4$year == yr_smpl),]
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
      df_sbs.MO4_03 <- df_MONIS4eDNA03[ which(df_MONIS4eDNA03$season== season), ]
      #check if the data frame is empty . See this question : https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
      #check across two statements
      if (dim(df_sbs.MO4_03)[1] == 0) {
        print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
        #}
        #if subsetted data fram for spring is empty - 
        #no samples
        #then create an empty data frame with zeroes and 
        #no color for points
        #
        plot.new()
      }
      # check for multiple situations - second check if the data frame has more than one dimension - https://www.datamentor.io/r-programming/if-else-statement/
      # if the dataframe does have more than one dimension, then try and plot it
      else if (dim(df_sbs.MO4_03)[1] >= 1)
        #start curly bracket for 'else if' test testing whether the dimensions on the data frame is more than one, if it is then make the plot on the map
      {
        
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lon <- min(df_sbs.MO4_03$declon)
        M27_jmax_lon <- max(df_sbs.MO4_03$declon)
        jit_lon <- (M27_jmax_lon-M27_jmin_lon)/8000
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lat <- min(df_sbs.MO4_03$declat)
        M27_jmax_lat <- max(df_sbs.MO4_03$declat)
        jit_lat <- (M27_jmax_lat-M27_jmin_lat)/8000
        #jitter points to work around overlapping points
        df_sbs.MO4_03$jit.lok_pos_lon <- jitter(df_sbs.MO4_03$declon, jit_lon)
        df_sbs.MO4_03$jit.lok_pos_lat <- jitter(df_sbs.MO4_03$declat, jit_lat)
        #make SpatialPointsDataFrame with decimal degree coordinates
        #make points with decimal coordinates - this makes use of a different CRS
        pnts2 <- SpatialPointsDataFrame(df_sbs.MO4_03[,c("jit.lok_pos_lon","jit.lok_pos_lat")],
                                        df_sbs.MO4_03,
                                        proj4string = crs(epsg4326nCRS2))
        #make points in decimal degr coordinates in a spatial dataframe for sampl locations
        #but moved a bit aside to plot eDNA evaluations - for the season
        df_sbs.MO4_03$lok_pos_lon.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lon + 0
        df_sbs.MO4_03$lok_pos_lat.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lat + 0.08
        
        #turn these new points in to a spatial data frame
        pnts4 <-
          SpatialPointsDataFrame(df_sbs.MO4_03[, c("lok_pos_lon.f.pnts4", "lok_pos_lat.f.pnts4")],
                                 df_sbs.MO4_03,
                                 proj4string = crs(epsg4326nCRS2))
        
        #prepare bounding box for decimal degrees
        bbox_k2 <- raster::buffer(
          as(extent(spTransform(pnts2, projection(coastline10))), "SpatialPolygons"),
          width = 7) # A width=10 zooms out and includes 
        #more sourrounding landmass in the map. 
        #A width=2 zooms in and includes less landmass 
        #around in the map.
        #project coastline on bbbox
        projection(bbox_k2) <- projection(coastline10)
        pols2 <- raster::crop(coastline10, bbox_k2)
        #transform to spdf with decim degr coordinates
        pols2 <- spTransform(pols2, projection(pnts2))
        
        #make costras with lon lat  coordinates
        costras2 <- costrasterGen(pnts2, pols2, extent = "polys",
                                  projstr = projection(pols2),
                                  resolution = res_fac)
        
        #resolution = 0.08) #Note this is in decimals for 
        #lonlat coordinates. Higher value (>1) makes 
        #interpolation goes fast, low (<0.05) takes too 
        #much RAM and too much time
        #make ipdw result 'res.ipdw2' based on pnts with 
        #lon-lat coordinates
        #This ipdw result 'res.ipdw2' is different in 
        #blending between colours between points
        #As compared to the section below that involves the 
        #'training' element in 'res.ipdw3'
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
        #if the ppp function complains about duplicated points,
        #you can check which ones are duplicated
        #duplicated(kat.pp)
        #go back and check your original lon-lat columns 
        #in the input data frame and make sure no lon-lat 
        #are duplicated.
        mean.neighdist <- mean(nndist(kat.pp))
        # grid building
        gridsize       <- mean.neighdist * 1*r_mnd #increasing the multiplier makes the gradients blend more across the borders equidistant from each point
        r_mnd1.lonlat       <- (mean.neighdist * 1*r_mnd)
        grainscale.fac <- gridsize / res(costras)[1]
        # notice that 'fact' option is required to be >0
        #you will have to adjust 'r_mnd' to make sure
        #"grainscale.fac" end up being lareger than 0
        gridras        <- aggregate(costras, fact = grainscale.fac)
        gridpol        <- rasterToPolygons(gridras)
        gridpol$value  <- row.names(gridpol)
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
          activesub <- subset(fulldataset.over, 
                              fulldataset.over$value == gridlev[i])
          selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
          if(i == 1){
            training <- activesub[selectnum,]
          }
          else{
            training <- rbind(training, activesub[selectnum,])
          }
        }
        #####
        validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
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
        pl <- c("log.10_copies_L")
        pl <- c("nosiHp_em")
        #make the ipdw raster
        res.ipdw3 <- ipdw(training, costras, range = mean.neighdist * 8*r_mnd, pl,
                          overlapped = TRUE, dist_power = 2)
        r_mnd2.lonlat <- (mean.neighdist * 4 *r_mnd)
        #low range reduces areas around point. #Note this is in decimals for lonlat coordinates.
        #increasing the 'dist_power' to 10 makes gradients between points less pronounced, and instead colors more intensely up to the borders
        #get color from the subsetted data frame
        col.f.ramp.pal <- "blue" # unique(loc_edna04$col_f_Phyl)
        col.f.ramp.pal <- "orange" #unique(loc_edna04$spcf_col_f_Phyl)
        #match species name in loop with the color for the species
        seasons.col.lst$color
        phy_col <- seasons.col.lst$color[match(season,
                        seasons.col.lst$season)]
        #okace the color in the object that is needed in 
        #the plot
        col.f.ramp.pal <- phy_col
        #make a colour range
        colfunc03 <-
          colorRampPalette(c("white", col.f.ramp.pal, "black"))
        Apts = 30
        #make color range
        cols01 <- colfunc03(30) #white to phylum-color to black
        #define limits to the axis of the plot
        xx <- labeling::extended(3, 17, 14, only.loose=TRUE)
        yy <- labeling::extended(54, 60,6, only.loose=TRUE)
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
             las=1) #las=1 turns the tick label on the axis
        #Add text along x- and y-axis in specified color and size
        mtext("longitude", side=1, line=3, col="black", cex=2)
        mtext("latitude", side=2, line=3, col="black", cex=2)
        #add land
        plot(pols2, add = TRUE, col="black", bg="azure4")
        #add land in a different colour
        plot(pols2, add = TRUE, col="azure4")
        #change font size on tick labels on axis
        axis(1, at = xx, cex.axis=2.0)
        axis(2, at = yy, las=1, cex.axis=2.0) #las=1 turns the labels at the tick marks on the axis
        #the tick marks on the axis
        
        #deduct a bit from the latitude, to lower the 
        #positioning of  the label
        redlatpt1 <- (max(as.data.frame(pnts4)$declat)-min(as.data.frame(pnts4)$declat))/350.6
        
        #head(as.data.frame(pnts4),6)
        #add text to the same lon lat position for harbour, or for month
        text(as.data.frame(pnts4)$declon,
             as.data.frame(pnts4)$declat-redlatpt1,
             #as.data.frame(pnts4)$Harbour.abbr1,
             #as.data.frame(pnts4)$tnosdbeDNA,
             as.data.frame(pnts4)$nosiHp_em,
             #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
             col="white",
             pos=1,
             cex= 2.4
        )
        #add text to the same lon lat position for harbour, or for month
        text(as.data.frame(pnts4)$declon,
             as.data.frame(pnts4)$declat-redlatpt1,
             #as.data.frame(pnts4)$Harbour.abbr1,
             #as.data.frame(pnts4)$tnosdbeDNA,
             as.data.frame(pnts4)$nosiHp_em,
             #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
             pos=1,
             cex= 1.8
        )
        #add a title for key-legend on side
        mtext("no of spc recorded by eDNA monit", side=4, line=5, cex=1.8)
        #add a title with bquote
        #use 'atop' function to get the title to appear on two lines
        #see this website: https://stackoverflow.com/questions/20549337/expression-and-new-line-in-plot-labels?lq=1
        title(main = c(bquote(
          atop(
            'eDNA no of spcs'
            #~ italic(.(spec.lat.no_undersc)),
            ~ ' for '
            ~ .(spcfc_seaon_name)
            ~ ' in '
            ~ .(yr_smpl)
            ~ '  ')
        )))

        #end curly bracket for 'else if' test testing 
        #whether the dimensions on the data frame is more 
        #than one, if it is then make the plot on the map
      }
      #end loop over seasons
    }
    # add title for the pdf-page
    mtext(
      c(paste("Appendix F", filnm, "."),  sep = ""),
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
    #end loop over year
  }

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the number of species detected with eDNA lvls above zero -  end
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::








#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# # Plot the number of species detected with conventional monitoring - start
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  ######
  # Exporting PFD files via postscript()
  filnm <- "no_spc_by_convmnt"
  #first make a file name
  flnm <- paste(
    wd00,wd10,"/suppmatr_10.12b_App_F_",filnm,
    "_ipwd_res",res_fac,"_mnd",
    r_mnd,
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
    
    #get maximum recorded number of species by eDNA method
    #per harbopur
    #get prefered upper limit of legend, rounded up 
    # - use later for zlim
    zvm<-ceiling(max(df_MONIS4$nosiHp_cm))
    
    #subset the original dataframe per year sampled
    #to be able to plot eDNA evaluation squares
    df_MONIS4eDNA03 <-
      df_MONIS4[which(df_MONIS4$year == yr_smpl),]
    # to test the loop with one variable  use a single selected season
    # season <- "season_2"
    #categories.of.seasons <-"season_1"
    #reverse the order of the elements in the vector
    # because you will want to have the plot with the ipdw
    #map for spring on the left, and the fall ipdw map on the right
    rctse <- rev(categories.of.seasons)
    rctse <- "efteraar"
    #iterate over the season in the vector
    for (season in rctse){
      print(season)
      #}
      #use match to match the season with a data frame and get the name for the season
      spcfc_seaon_name <- seaons_nms_df$names.of.seasons[match(season, seaons_nms_df$categories.of.seasons)]
      spcfc_seaon_name <- as.character(spcfc_seaon_name)
      
      # subset among the months to get from 1 to 6
      df_sbs.MO4_03 <- df_MONIS4eDNA03[ which(df_MONIS4eDNA03$season== season), ]
      #check if the data frame is empty . See this question : https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
      #check across two statements
      if (dim(df_sbs.MO4_03)[1] == 0) {
        print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
        #}
        #if subsetted data fram for spring is empty - 
        #no samples
        #then create an empty data frame with zeroes and 
        #no color for points
        #
        plot.new()
      }
      # check for multiple situations - second check if the data frame has more than one dimension - https://www.datamentor.io/r-programming/if-else-statement/
      # if the dataframe does have more than one dimension, then try and plot it
      else if (dim(df_sbs.MO4_03)[1] >= 1)
        #start curly bracket for 'else if' test testing whether the dimensions on the data frame is more than one, if it is then make the plot on the map
      {
        
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lon <- min(df_sbs.MO4_03$declon)
        M27_jmax_lon <- max(df_sbs.MO4_03$declon)
        jit_lon <- (M27_jmax_lon-M27_jmin_lon)/8000
        #get minimum and maximum to define range for jitter of points
        M27_jmin_lat <- min(df_sbs.MO4_03$declat)
        M27_jmax_lat <- max(df_sbs.MO4_03$declat)
        jit_lat <- (M27_jmax_lat-M27_jmin_lat)/8000
        #jitter points to work around overlapping points
        df_sbs.MO4_03$jit.lok_pos_lon <- jitter(df_sbs.MO4_03$declon, jit_lon)
        df_sbs.MO4_03$jit.lok_pos_lat <- jitter(df_sbs.MO4_03$declat, jit_lat)
        #make SpatialPointsDataFrame with decimal degree coordinates
        #make points with decimal coordinates - this makes use of a different CRS
        pnts2 <- SpatialPointsDataFrame(df_sbs.MO4_03[,c("jit.lok_pos_lon","jit.lok_pos_lat")],
                                        df_sbs.MO4_03,
                                        proj4string = crs(epsg4326nCRS2))
        #points without jitter
        # pnts2 <- SpatialPointsDataFrame(df_sbs.MO4_03[,c("declon","declat")],
        #                                 df_sbs.MO4_03,
        #                                 proj4string = crs(epsg4326nCRS2))
        # 
        
        #make points in decimal degr coordinates in a spatial dataframe for sampl locations
        #but moved a bit aside to plot eDNA evaluations - for the season
        df_sbs.MO4_03$lok_pos_lon.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lon + 0
        df_sbs.MO4_03$lok_pos_lat.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lat + 0.08
        
        #turn these new points in to a spatial data frame
        pnts4 <-
          SpatialPointsDataFrame(df_sbs.MO4_03[, c("lok_pos_lon.f.pnts4", "lok_pos_lat.f.pnts4")],
                                 df_sbs.MO4_03,
                                 proj4string = crs(epsg4326nCRS2))
        
        #prepare bounding box for decimal degrees
        bbox_k2 <- raster::buffer(
          as(extent(spTransform(pnts2, projection(coastline10))), "SpatialPolygons"),
          width = 7) # A width=10 zooms out and includes 
        #more sourrounding landmass in the map. 
        #A width=2 zooms in and includes less landmass 
        #around in the map.
        #project coastline on bbbox
        projection(bbox_k2) <- projection(coastline10)
        pols2 <- raster::crop(coastline10, bbox_k2)
        #transform to spdf with decim degr coordinates
        pols2 <- spTransform(pols2, projection(pnts2))
        
        #make costras with lon lat  coordinates
        costras2 <- costrasterGen(pnts2, pols2, extent = "polys",
                                  projstr = projection(pols2),
                                  resolution = res_fac)
        
        #resolution = 0.08) #Note this is in decimals for 
        #lonlat coordinates. Higher value (>1) makes 
        #interpolation goes fast, low (<0.05) takes too 
        #much RAM and too much time
        #make ipdw result 'res.ipdw2' based on pnts with 
        #lon-lat coordinates
        #This ipdw result 'res.ipdw2' is different in 
        #blending between colours between points
        #As compared to the section below that involves the 
        #'training' element in 'res.ipdw3'
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
        #if the ppp function complains about duplicated points,
        #you can check which ones are duplicated
        #duplicated(kat.pp)
        #go back and check your original lon-lat columns 
        #in the input data frame and make sure no lon-lat 
        #are duplicated.
        mean.neighdist <- mean(nndist(kat.pp))
        # grid building
        gridsize       <- mean.neighdist * 1*r_mnd #increasing the multiplier makes the gradients blend more across the borders equidistant from each point
        r_mnd1.lonlat       <- (mean.neighdist * 1*r_mnd)
        grainscale.fac <- gridsize / res(costras)[1]
        # notice that 'fact' option is required to be >0
        #you will have to adjust 'r_mnd' to make sure
        #"grainscale.fac" end up being lareger than 0
        gridras        <- aggregate(costras, fact = grainscale.fac)
        gridpol        <- rasterToPolygons(gridras)
        gridpol$value  <- row.names(gridpol)
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
          activesub <- subset(fulldataset.over, 
                              fulldataset.over$value == gridlev[i])
          selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
          if(i == 1){
            training <- activesub[selectnum,]
          }
          else{
            training <- rbind(training, activesub[selectnum,])
          }
        }
        #####
        validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
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
        pl <- c("log.10_copies_L")
        #pl <- c("nosiHp_em")
        pl <- c("nosiHp_cm")
        #make the ipdw raster
        res.ipdw3 <- ipdw(training, costras, range = mean.neighdist * 8*r_mnd, pl,
                          overlapped = TRUE, dist_power = 2.0)
        r_mnd2.lonlat <- (mean.neighdist * 6 *r_mnd)
        #low range reduces areas around point. #Note this is in decimals for lonlat coordinates.
        #increasing the 'dist_power' to 10 makes gradients between points less pronounced, and instead colors more intensely up to the borders
        #get color from the subsetted data frame
        col.f.ramp.pal <- "blue" # unique(loc_edna04$col_f_Phyl)
        col.f.ramp.pal <- "orange" #unique(loc_edna04$spcf_col_f_Phyl)
        #match species name in loop with the color for the species
        seasons.col.lst$color
        phy_col <- seasons.col.lst$color[match(season,
                                               seasons.col.lst$season)]
        #okace the color in the object that is needed in 
        #the plot
        col.f.ramp.pal <- phy_col
        col.f.ramp.pal <- "pink"
        #make a colour range
        colfunc03 <-
          colorRampPalette(c("white", col.f.ramp.pal, "firebrick4"))
        Apts = 30
        #make color range
        cols01 <- colfunc03(30) #white to phylum-color to black
        #define limits to the axis of the plot
        xx <- labeling::extended(3, 17, 14, only.loose=TRUE)
        yy <- labeling::extended(54, 60,6, only.loose=TRUE)
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
             las=1) #las=1 turns the tick label on the axis
        #Add text along x- and y-axis in specified color and size
        mtext("longitude", side=1, line=3, col="black", cex=2)
        mtext("latitude", side=2, line=3, col="black", cex=2)
        #add land
        plot(pols2, add = TRUE, col="black", bg="azure4")
        #add land in a different colour
        plot(pols2, add = TRUE, col="azure4")
        #add special ticks and adjust the size of the text associated 
        #with these tick marks
        axis(1, at = xx, cex.axis=2.0)
        axis(2, at = yy, las=1, cex.axis=2.0) #las=1 turns the labels at the tick marks on the axis
        #set range for z key
        #the tick marks on the axis
        
        #deduct a bit from the latitude, to lower the 
        #positioning of  the label
        redlatpt1 <- (max(as.data.frame(pnts4)$declat)-min(as.data.frame(pnts4)$declat))/350.6
        #head(as.data.frame(pnts4),6)
        # #add text to the same lon lat position for harbour, or for month
        text(as.data.frame(pnts4)$declon,
             as.data.frame(pnts4)$declat-redlatpt1,
             #as.data.frame(pnts4)$Harbour.abbr1,
             #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
             as.data.frame(pnts4)$nosiHp_cm,
             col="white",
             pos=1,
             cex= 2.4
        )
        # #add text to the same lon lat position for harbour, or for month
        text(as.data.frame(pnts4)$declon,
             as.data.frame(pnts4)$declat-redlatpt1,
             #as.data.frame(pnts4)$Harbour.abbr1,
             #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
             as.data.frame(pnts4)$nosiHp_cm,
             pos=1,
             cex= 1.8
        )
        #add a title for key-legend on side
        mtext("no of spc recorded by conv monit", side=4, line=5, cex=1.8)
        #add a title with bquote
        #use 'atop' function to get the title to appear on two lines
        #see this website: https://stackoverflow.com/questions/20549337/expression-and-new-line-in-plot-labels?lq=1
        title(main = c(bquote(
          atop(
            'conv monit no of spcs'
            #~ italic(.(spec.lat.no_undersc)),
            ~ ' for '
            ~ .(spcfc_seaon_name)
            ~ ' in '
            ~ .(yr_smpl)
            ~ '  ')
        )))
        
        #end curly bracket for 'else if' test testing 
        #whether the dimensions on the data frame is more 
        #than one, if it is then make the plot on the map
      }
      #end loop over seasons
    }
    # add title for the pdf-page
    mtext(
      c(paste("Appendix F", filnm, "."),  sep = ""),
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
    #end loop over year
  }
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the number of species detected with conventional monitoring -  end
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Find the highest eDNA level recorded per species per harbour
# for the two seasons - then afterwards subset the dataframe
#  and plot the highest number of species recorded per 
# harbour by eDNA over both seasons

#count the number of harbours per season  
tbl_MONIS4_ch02 <- dplyr::count(df_MONIS4, Harbour.season, eDNA_presence1)
#make it a data frame
df_MONIS4_ch02 <-  as.data.frame(tbl_MONIS4_ch02)
#subset to only include the 'presence' of species counts
df_MONIS4_ch03 <- subset(df_MONIS4_ch02, eDNA_presence1==1)
#split string by point, bind the result as rows, 
#get the first column, which is the harbours, 
#and add back to the original data frame
df_MONIS4_ch03$Harbour <- data.frame(do.call('rbind', strsplit(as.character(df_MONIS4_ch03$Harbour.season),'.',fixed=TRUE)))[,1]
# Use  dplyr to get maximum value within a group:
# see here: https://stackoverflow.com/questions/25314336/extract-the-maximum-value-within-each-group-in-a-dataframe
tbl_MONIS4_ch03 <- df_MONIS4_ch03 %>% dplyr::group_by(Harbour) %>% dplyr::summarise(Value = max(n))
#make it a data frame
df_MONIS4_ch04 <-  as.data.frame(tbl_MONIS4_ch03)
#change the column name to total no of spc detected by eDNA
colnames(df_MONIS4_ch04)[2] <- "tnosdbeDNA"
#match this back to the original data frame imported
df_MONIS4$tnosdbeDNA <- df_MONIS4_ch04$tnosdbeDNA[match(df_MONIS4$Harbour,df_MONIS4_ch04$Harbour)]

#Now make the ipdw plot with maximum number of species 
#per season per harbour

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# # Plot the total number of species detected with eDNA monitoring - start
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

######
# Exporting PFD files via postscript()
filnm <- "tot_spc_by_eDNAmnt"
#first make a file name
flnm <- paste(
  wd00,wd10,"/suppmatr_10.12b_App_F_",filnm,
  "_ipwd_res",res_fac,"_mnd",
  r_mnd,
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
  
  #get maximum recorded number of species by eDNA method
  #per harbopur
  #get prefered upper limit of legend, rounded up 
  # - use later for zlim
  zvm<-ceiling(max(df_MONIS4$tnosdbeDNA))
  
  #subset the original dataframe per year sampled
  #to be able to plot eDNA evaluation squares
  df_MONIS4eDNA03 <-
    df_MONIS4[which(df_MONIS4$year == yr_smpl),]
  # to test the loop with one variable  use a single selected season
  # season <- "season_2"
  #categories.of.seasons <-"season_1"
  #reverse the order of the elements in the vector
  # because you will want to have the plot with the ipdw
  #map for spring on the left, and the fall ipdw map on the right
  rctse <- rev(categories.of.seasons)
  rctse <- "efteraar"
  #iterate over the season in the vector
  for (season in rctse){
    print(season)
    #}
    #use match to match the season with a data frame and get the name for the season
    spcfc_seaon_name <- seaons_nms_df$names.of.seasons[match(season, seaons_nms_df$categories.of.seasons)]
    spcfc_seaon_name <- as.character(spcfc_seaon_name)
    
    # subset among the months to get from 1 to 6
    df_sbs.MO4_03 <- df_MONIS4eDNA03[ which(df_MONIS4eDNA03$season== season), ]
    #check if the data frame is empty . See this question : https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty
    #check across two statements
    if (dim(df_sbs.MO4_03)[1] == 0) {
      print(paste("data frame for",spcfc_seaon_name,yr_smpl,"is empty", sep=" "))
      #}
      #if subsetted data fram for spring is empty - 
      #no samples
      #then create an empty data frame with zeroes and 
      #no color for points
      #
      plot.new()
    }
    # check for multiple situations - second check if the data frame has more than one dimension - https://www.datamentor.io/r-programming/if-else-statement/
    # if the dataframe does have more than one dimension, then try and plot it
    else if (dim(df_sbs.MO4_03)[1] >= 1)
      #start curly bracket for 'else if' test testing whether the dimensions on the data frame is more than one, if it is then make the plot on the map
    {
      
      #get minimum and maximum to define range for jitter of points
      M27_jmin_lon <- min(df_sbs.MO4_03$declon)
      M27_jmax_lon <- max(df_sbs.MO4_03$declon)
      jit_lon <- (M27_jmax_lon-M27_jmin_lon)/8000
      #get minimum and maximum to define range for jitter of points
      M27_jmin_lat <- min(df_sbs.MO4_03$declat)
      M27_jmax_lat <- max(df_sbs.MO4_03$declat)
      jit_lat <- (M27_jmax_lat-M27_jmin_lat)/8000
      #jitter points to work around overlapping points
      df_sbs.MO4_03$jit.lok_pos_lon <- jitter(df_sbs.MO4_03$declon, jit_lon)
      df_sbs.MO4_03$jit.lok_pos_lat <- jitter(df_sbs.MO4_03$declat, jit_lat)
      #make SpatialPointsDataFrame with decimal degree coordinates
      #make points with decimal coordinates - this makes use of a different CRS
      pnts2 <- SpatialPointsDataFrame(df_sbs.MO4_03[,c("jit.lok_pos_lon","jit.lok_pos_lat")],
                                      df_sbs.MO4_03,
                                      proj4string = crs(epsg4326nCRS2))
      #points without jitter
      # pnts2 <- SpatialPointsDataFrame(df_sbs.MO4_03[,c("declon","declat")],
      #                                 df_sbs.MO4_03,
      #                                 proj4string = crs(epsg4326nCRS2))
      # 
      
      #make points in decimal degr coordinates in a spatial dataframe for sampl locations
      #but moved a bit aside to plot eDNA evaluations - for the season
      df_sbs.MO4_03$lok_pos_lon.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lon + 0
      df_sbs.MO4_03$lok_pos_lat.f.pnts4 <- df_sbs.MO4_03$jit.lok_pos_lat + 0.08
      
      #turn these new points in to a spatial data frame
      pnts4 <-
        SpatialPointsDataFrame(df_sbs.MO4_03[, c("lok_pos_lon.f.pnts4", "lok_pos_lat.f.pnts4")],
                               df_sbs.MO4_03,
                               proj4string = crs(epsg4326nCRS2))
      
      #prepare bounding box for decimal degrees
      bbox_k2 <- raster::buffer(
        as(extent(spTransform(pnts2, projection(coastline10))), "SpatialPolygons"),
        width = 7) # A width=10 zooms out and includes 
      #more sourrounding landmass in the map. 
      #A width=2 zooms in and includes less landmass 
      #around in the map.
      #project coastline on bbbox
      projection(bbox_k2) <- projection(coastline10)
      pols2 <- raster::crop(coastline10, bbox_k2)
      #transform to spdf with decim degr coordinates
      pols2 <- spTransform(pols2, projection(pnts2))
      
      #make costras with lon lat  coordinates
      costras2 <- costrasterGen(pnts2, pols2, extent = "polys",
                                projstr = projection(pols2),
                                resolution = res_fac)
      
      #resolution = 0.08) #Note this is in decimals for 
      #lonlat coordinates. Higher value (>1) makes 
      #interpolation goes fast, low (<0.05) takes too 
      #much RAM and too much time
      #make ipdw result 'res.ipdw2' based on pnts with 
      #lon-lat coordinates
      #This ipdw result 'res.ipdw2' is different in 
      #blending between colours between points
      #As compared to the section below that involves the 
      #'training' element in 'res.ipdw3'
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
      #if the ppp function complains about duplicated points,
      #you can check which ones are duplicated
      #duplicated(kat.pp)
      #go back and check your original lon-lat columns 
      #in the input data frame and make sure no lon-lat 
      #are duplicated.
      mean.neighdist <- mean(nndist(kat.pp))
      # grid building
      gridsize       <- mean.neighdist * 1*r_mnd #increasing the multiplier makes the gradients blend more across the borders equidistant from each point
      r_mnd1.lonlat       <- (mean.neighdist * 1*r_mnd)
      grainscale.fac <- gridsize / res(costras)[1]
      # notice that 'fact' option is required to be >0
      #you will have to adjust 'r_mnd' to make sure
      #"grainscale.fac" end up being lareger than 0
      gridras        <- aggregate(costras, fact = grainscale.fac)
      gridpol        <- rasterToPolygons(gridras)
      gridpol$value  <- row.names(gridpol)
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
        activesub <- subset(fulldataset.over, 
                            fulldataset.over$value == gridlev[i])
        selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
        if(i == 1){
          training <- activesub[selectnum,]
        }
        else{
          training <- rbind(training, activesub[selectnum,])
        }
      }
      #####
      validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
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
      #pl <- c("log.10_copies_L")
      #pl <- c("nosiHp_em")
      #pl <- c("nosiHp_cm")
      pl <- c("tnosdbeDNA")
      #make the ipdw raster
      res.ipdw3 <- ipdw(training, costras, range = mean.neighdist * 8*r_mnd, pl,
                        overlapped = TRUE, dist_power = 2.0)
      r_mnd2.lonlat <- (mean.neighdist * 6 *r_mnd)
      #low range reduces areas around point. #Note this is in decimals for lonlat coordinates.
      #increasing the 'dist_power' to 10 makes gradients between points less pronounced, and instead colors more intensely up to the borders
      #get color from the subsetted data frame
      col.f.ramp.pal <- "blue" # unique(loc_edna04$col_f_Phyl)
      col.f.ramp.pal <- "orange" #unique(loc_edna04$spcf_col_f_Phyl)
      #match species name in loop with the color for the species
      seasons.col.lst$color
      phy_col <- seasons.col.lst$color[match(season,
                                             seasons.col.lst$season)]
      #okace the color in the object that is needed in 
      #the plot
      col.f.ramp.pal <- phy_col
      col.f.ramp.pal <- "yellow"
      #make a colour range
      colfunc03 <-
        colorRampPalette(c("white", col.f.ramp.pal, "sienna4"))
      Apts = 30
      #make color range
      cols01 <- colfunc03(30) #white to phylum-color to black
      #define limits to the axis of the plot
      xx <- labeling::extended(3, 17, 14, only.loose=TRUE)
      yy <- labeling::extended(54, 60,6, only.loose=TRUE)
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
           las=1) #las=1 turns the tick label on the axis
      #add land
      plot(pols2, add = TRUE, col="black", bg="azure4")
      #add land in a different colour
      plot(pols2, add = TRUE, col="azure4")
      #Add text along x- and y-axis in specified color and size
      mtext("longitude", side=1, line=3, col="black", cex=2)
      mtext("latitude", side=2, line=3, col="black", cex=2)
      #add special ticks and adjust the size of the text associated 
      #with these tick marks
      axis(1, at = xx, cex.axis=2.0)
      axis(2, at = yy, las=1, cex.axis=2.0) #las=1 turns the labels at the tick marks on the axis
      #the tick marks on the axis
      
      #deduct a bit from the latitude, to lower the 
      #positioning of  the label
      redlatpt1 <- (max(as.data.frame(pnts4)$declat)-min(as.data.frame(pnts4)$declat))/350.6
      # #add text to the same lon lat position for harbour, or for month
      text(as.data.frame(pnts4)$declon,
           as.data.frame(pnts4)$declat-redlatpt1,
           #as.data.frame(pnts4)$Harbour.abbr1,
           as.data.frame(pnts4)$tnosdbeDNA,
           #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
           pos=1,
           col="white",
           cex= 2.4
      )
      # #add text to the same lon lat position for harbour, or for month
      text(as.data.frame(pnts4)$declon,
           as.data.frame(pnts4)$declat-redlatpt1,
           #as.data.frame(pnts4)$Harbour.abbr1,
           as.data.frame(pnts4)$tnosdbeDNA,
           #as.data.frame(pnts4)$month2, # use this one instead if you want the sampling month underneath
           pos=1,
           cex= 1.8
      )
      #add a title for key-legend on side
      mtext("total no of spc recorded by eDNA monit", side=4, line=5, cex=1.8)
      #add a title with bquote
      #use 'atop' function to get the title to appear on two lines
      #see this website: https://stackoverflow.com/questions/20549337/expression-and-new-line-in-plot-labels?lq=1
      title(main = c(bquote(
        atop(
          'tot eDNA monit no of spcs'
          #~ italic(.(spec.lat.no_undersc)),
          ~ ' for '
          ~ .(spcfc_seaon_name)
          ~ ' in '
          ~ .(yr_smpl)
          ~ '  ')
      )))
      
      #end curly bracket for 'else if' test testing 
      #whether the dimensions on the data frame is more 
      #than one, if it is then make the plot on the map
    }
    #end loop over seasons
  }
  # add title for the pdf-page
  mtext(
    c(paste("Appendix F", filnm, "."),  sep = ""),
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
  #end loop over year
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the total number of species detected with eDNA monitoring -  end
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#define output file and path
outfile01 <- paste(wd00,wd10,"/suppmatr_10.12c_MONIS4eDNA02_df.csv",sep="")
#write the table to a csv
#to be used for the next R-code that plots eDNA levels on maps
write.csv(df_MONIS4, file = outfile01)



#