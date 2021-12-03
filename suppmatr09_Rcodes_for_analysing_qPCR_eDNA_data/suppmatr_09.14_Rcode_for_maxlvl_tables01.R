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
  rm(list=ls())
  
  #install packages
  if(!require(magrittr)){
    install.packages("magrittr")
  }
  if(!require(dplyr)){
    install.packages("dplyr")
  }
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
  
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
  #the input file prepared with the previous code
  in.csv.file01 <- paste(wd00,wd10,"/suppmatr_10.12c_MONIS4eDNA02_df.csv",sep="")
  #read in csv file from previous code
  df_MONIS4_02 <-as.data.frame(read.csv(in.csv.file01,
                                     header = TRUE, sep = ",", quote = "\"",
                                     dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))
  # define columns to keep
  keep <- c("no_for_log.10_eDNAlvls","spc","season","Harbour")
  #keep only selected columns
  df_MONIS4_03 <- df_MONIS4_02[keep]
  #paste two columns together to use later for reshaping the data
  # frame
  df_MONIS4_03$spc.Harbour <- paste(df_MONIS4_03$spc,df_MONIS4_03$Harbour,sep=".")  
  #reshape the data frame by the new column, and make new columns
  # based on the 'timevar' argument, here there are two seasons
  # so two columns are added for the variable with eDNA scores
  # one column for each of the two season
  df_MONIS4_04 <- reshape(df_MONIS4_03,idvar="spc.Harbour",timevar="season",direction="wide")
  #get the highest value between two columns
  #https://stackoverflow.com/questions/28531809/i-want-to-select-the-greater-of-the-two-values-from-two-columns-in-r
  df_MONIS4_04$max.log10_eDNA_lvl_perseason <- pmax(df_MONIS4_04$no_for_log.10_eDNAlvls.foraar, df_MONIS4_04$no_for_log.10_eDNAlvls.efteraar)
  #match to data frame and append as a new column
  df_MONIS4_02$max.log10_eDNA_lvl_perseason <- df_MONIS4_04$max.log10_eDNA_lvl_perseason[match(df_MONIS4_02$spc.Harbour,df_MONIS4_04$spc.Harbour)]
  
  # Retain the highest value per row in a group
  #https://stackoverflow.com/questions/24070714/extract-row-corresponding-to-minimum-value-of-a-variable-by-group
  require(dplyr) #make sure you have dplyr
  #first group by 'spc.Harbour', then find the highest value within this group in 'no_for_log.10_eDNAlvls'
  tibl_MONIS4_05 <- dplyr::group_by(df_MONIS4_02, spc.Harbour) %>% dplyr::slice(which.max(no_for_log.10_eDNAlvls))
  #make it a data frame
  df_MONIS4_06 <- as.data.frame(tibl_MONIS4_05)
  
  #Replace old species name
  df_MONIS4_06$spc <- gsub("Crassostrea","Magallanas",df_MONIS4_06$spc)
  #use string split to get genus name and species name, and paste together
  genabbr <-  substr(data.frame(do.call('rbind', strsplit(as.character(df_MONIS4_06$spc),'_',fixed=TRUE)))[,1],1,1)
  spcnm <-    data.frame(do.call('rbind', strsplit(as.character(df_MONIS4_06$spc),'_',fixed=TRUE)))[,2]
  df_MONIS4_06$spc <- paste(genabbr,". ",spcnm,sep="")
  # sort the data frame by the column with species names
  df_MONIS4_06 <- df_MONIS4_06[order(df_MONIS4_06$spc), ]
  
  #select columns to keep
  keep <- c("Harbour","Harbour.abbr1")
  #only keep selected columns, only get unique entries
  df_ha01 <- unique(df_MONIS4_06[keep])
  
  #_____________________________________________________________
  # Rearrange the data frame to prepare it for a html table 
  # - part 01
  #_____________________________________________________________
  #define which columns to keep
  keep <- c("spc", "Harbour.abbr1","eDNA_eval_p_repl_descr")
  #keep only selected columns
  df_MONIS4_07 <- df_MONIS4_06[keep]
  #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  #reshape the data frame from long to wide
  df_MONIS4_08 <- reshape(df_MONIS4_07,timevar="spc",idvar="Harbour.abbr1",direction="wide")
  #substitute in the column names
  colnames(df_MONIS4_08) <- gsub("eDNA_eval_p_repl_descr\\.","",colnames(df_MONIS4_08))
  #transpose the data frame
  df_MONIS4_09 <- as.data.frame(t(df_MONIS4_08))
  #add a column with row names
  #df_MONIS4_09$spc <- row.names(df_MONIS4_09)
  
  #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
  if(!require(tableHTML)){
    install.packages("tableHTML")
    library(tableHTML)
  }
  require(tableHTML)
  #copy the data frame
  sbs.MO_df <- df_MONIS4_09
  #replace across the full dataframe
  #https://code-examples.net/en/q/13cc7e3
  # sbs.MO02_df <- as.data.frame(apply(sbs.MO_df,2,function(x)gsub('NoCt', 'NoCq',x)))
  # sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('1aboveLOQ', '1aLOQ',x)))
  # sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('3aboveLOQ', '3aLOQ',x)))
  # sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('AbLOD_BeLOQ', 'aLODbLOQ',x)))
  # sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('belowLOD', 'bLOD',x)))
  #replace across the full dataframe
  #use these abbreviations to get very short abbreviations
  #https://code-examples.net/en/q/13cc7e3
  sbs.MO02_df <- as.data.frame(apply(sbs.MO_df,2,function(x)gsub('NoCt', 'NC',x)))
  sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('1aboveLOQ', '1aL',x)))
  sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('3aboveLOQ', '3aL',x)))
  sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('AbLOD_BeLOQ', 'aLbL',x)))
  sbs.MO02_df <- as.data.frame(apply(sbs.MO02_df,2,function(x)gsub('belowLOD', 'bL',x)))
  
  
  sbs.MO_df <- sbs.MO02_df
  #head(sbs.MO_df,4)
  
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
  #make lists of the words in the cells to color using the 'add_css_conditional_column' function
  eDNA.lvl01 <- c("NC") #white
  eDNA.lvl02 <- c("bL") #yellow
  eDNA.lvl03 <- c("aLbL") #orange
  eDNA.lvl04 <- c("1aL") #red
  eDNA.lvl05 <- c("3aL") #black
  
  #exclude rows where row names match species that should be excluded
  sbs.MO_df <- sbs.MO_df[!row.names(sbs.MO_df)=="Paralithodes_camtschaticus", ]
  sbs.MO_df <- sbs.MO_df[!row.names(sbs.MO_df)=="Homarus_americanus", ]
  
  #copy the data frame
  df_sbs.MO.01 <- sbs.MO_df
  # sum up the count per column , but exclude counting the first row, as
  # this holds harbour abbreviations
  df_sbs.MO.01[nrow(df_sbs.MO.01)+1,] <- colSums(!df_sbs.MO.01[-1,]=="NC")
  #get the number of rows
  nr_df_sbs <- length(row.names(df_sbs.MO.01))
  #get the last row in the df, and get the row name, and change the row name
  row.names(df_sbs.MO.01)[nr_df_sbs] <- "total detections"
  #place the data frame in a tableHTML object
  tableHTML <- df_sbs.MO.01 %>% 
    tableHTML()
  # for eDNA.lvl01 <- c("NoCq") #white
  words <- eDNA.lvl01 #<- c("NoCq") #white
  col.f.cell <- "white"
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "==",
                                 value = word,
                                 css = list(c("background-color"),
                                            c(col.f.cell)))
  }
  # for eDNA.lvl03 <- c("abLODbeLOQ") #orange
  words <- eDNA.lvl03
  col.f.cell <- "orange"
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "==",
                                 value = word,
                                 css = list(c("background-color"),
                                            c(col.f.cell)))
  }
  # for eDNA.lvl04 <- c("1abLOQ") #red
  words <- eDNA.lvl04
  col.f.cell <- "red"
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "contains",
                                 value = word,
                                 css = list(c("background-color","color"),
                                            c(col.f.cell,col.f.font)))
  }
  
  t.HTML17 <- tableHTML
  #and to export in a file
  write_tableHTML(t.HTML17, file = paste(wd00,wd10,"/","suppmatr_10.14b_App_G_table_eDNA_max_evalu.html", sep=""))
  
  
  
  
  
  
  #_____________________________________________________________
  # Rearrange the data frame to prepare it for a html table 
  # - part 02
  #_____________________________________________________________
  #define which columns to keep
  keep <- c("spc", "Harbour.abbr1","freq_repl_eval")
  #keep only selected columns
  df_MONIS4_07 <- df_MONIS4_06[keep]
  #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  #reshape the data frame from long to wide
  df_MONIS4_08 <- reshape(df_MONIS4_07,timevar="spc",idvar="Harbour.abbr1",direction="wide")
  #substitute in the column names
  colnames(df_MONIS4_08) <- gsub("freq_repl_eval\\.","",colnames(df_MONIS4_08))
  #transpose the data frame
  df_MONIS4_09 <- as.data.frame(t(df_MONIS4_08))
  #add a column with row names
  #df_MONIS4_09$spc <- row.names(df_MONIS4_09)
  
  #get the number of columns
  nc.s09 <- ncol(df_MONIS4_09)
  # get unique values in the data frame from a selected 
  #range of columns # see : https://stackoverflow.com/questions/40003028/extracting-unique-values-from-data-frame-using-r
  unqval.s09 <- unique(unlist((df_MONIS4_09)[,2:nc.s09]))
  #remove NA from vector - this will exclude the 
  #non-evaluated categories
  unqval.s09 <- unqval.s09[!is.na(unqval.s09)]
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
  grep("^'0", unqval.s09, value=T)
  #grep for elements that end with 0'
  grep("0'$", unqval.s09, value=T)
  # grep for elements starting '0/0/0/ and then 1 or any higher number
  b_cat <- grep("'0/0/0/[1-9]+", unqval.s09, value=T) # this will equal the black category with all replicates amplyfiying
  # then grep for all elements with not zero in the last category
  br_cat <- grep("'[0-9]+/[0-9]+/[0-9]+/[1-9]+", unqval.s09, value=T) # this will equal both the red and balck category
  # find the difference between these two vectors - i.e. subtract the black category from the fused red-black category
  r_cat <- setdiff(br_cat,b_cat)
  # grep for all elements with not zero in the first category
  w_cat <- grep("'[1-9]+/0/0/0'", unqval.s09, value=T) # this will equal all replicates not amplifying - i.e. this will equal the white category
  # find the difference between two vectors
  wyo_cat <- setdiff(unqval.s09,br_cat)
  # find the difference between two vectors
  yo_cat <- setdiff(wyo_cat,w_cat)
  # grep for all elements with not zero in the last and second last category
  y_cat <- grep("'[0-9]+/[1-9]+/0/0'", unqval.s09, value=T)
  # grep for all elements with not zero in thelast category
  o_cat <- grep("'[0-9]+/[0-9]+/[1-9]+/0'", unqval.s09, value=T)
  #these categories are used below to identify the color coding in the html tables
  
  
  #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
  if(!require(tableHTML)){
    install.packages("tableHTML")
    library(tableHTML)
  }
  require(tableHTML)
  sbs.MO_df <- df_MONIS4_09
  #exclude rows where row names match species that should be excluded
  sbs.MO_df <- sbs.MO_df[!row.names(sbs.MO_df)=="Paralithodes_camtschaticus", ]
  sbs.MO_df <- sbs.MO_df[!row.names(sbs.MO_df)=="Homarus_americanus", ]
  #copy the data frame
  df_sbs.MO.01 <- sbs.MO_df
  # sum up the count per column , but exclude counting the first row, as
  # this holds harbour abbreviations
  df_sbs.MO.01[nrow(df_sbs.MO.01)+1,] <- colSums(!df_sbs.MO.01[-1,]=="'3/0/0/0'")
  #get the number of rows
  nr_df_sbs <- length(row.names(df_sbs.MO.01))
  #get the last row in the df, and get the row name, and change the row name
  row.names(df_sbs.MO.01)[nr_df_sbs] <- "total detections"
  
  #try the tableHTML with no border
  tableHTML <- df_sbs.MO.01 %>% 
    tableHTML(border = 0) 
  #count the number of columns in the dataframe
  l.s.MO <- length(df_sbs.MO.01)
  #get unique cell values in dataframe : see : http://r.789695.n4.nabble.com/Retrieve-distinct-values-within-a-whole-data-frame-td1460205.html
  #apart from the first column
  unique(unlist(df_sbs.MO.01[2:l.s.MO]))
  class(eDNA.lvl01)
  eDNA.lvl01 <- w_cat #c("/0/0/0'") #white
  eDNA.lvl02 <- y_cat #c("'0/") #yellow
  eDNA.lvl03 <- o_cat #c("aLODbLOQ") #orange
  eDNA.lvl04 <- r_cat #c("/1'") #red
  eDNA.lvl05 <- b_cat #c("'0/0/0/") #black
  #place the data frame in a tableHTML object
  tableHTML <- df_sbs.MO.01 %>% 
    tableHTML()
  # for eDNA.lvl01 <- c("NoCq") #white
  words <- eDNA.lvl01 #<- c("NoCq") #white
  col.f.cell <- "white"
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "contains",
                                 value = word,
                                 css = list(c("background-color","color"),
                                            c(col.f.cell,col.f.font)))
  }
  
  t.HTML17 <- tableHTML
  #and to export in a file
  write_tableHTML(t.HTML17, file = paste(wd00,wd10,"/","suppmatr_10.14c_App_G_table_eDNA_max_qpcrreplic.html", sep=""))
  
  
  
  
  
  
  
  #_____________________________________________________________
  # Rearrange the data frame to prepare it for a html table 
  # - part 03
  #_____________________________________________________________
  #define which columns to keep
  keep <- c("spc", "Harbour.abbr1","categ_valid")
  #keep only selected columns
  df_MONIS4_07 <- df_MONIS4_06[keep]
  #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  #reshape the data frame from long to wide
  df_MONIS4_08 <- reshape(df_MONIS4_07,timevar="spc",idvar="Harbour.abbr1",direction="wide")
  #substitute in the column names
  colnames(df_MONIS4_08) <- gsub("categ_valid\\.","",colnames(df_MONIS4_08))
  #transpose the data frame
  df_MONIS4_09 <- as.data.frame(t(df_MONIS4_08))
  #add a column with row names
  #df_MONIS4_09$spc <- row.names(df_MONIS4_09)
  head(df_MONIS4_09,5)
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
  
  #replace across the full dataframe
  #use these abbreviations to get very short abbreviations
  #https://code-examples.net/en/q/13cc7e3
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('NoCt', 'NC',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('1AbLOQ', '1aL',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('3AbLOQ', '3aL',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('AbLOD_BeLOQ', 'aLbL',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('AbLOD', 'aLbL',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('BeLOD', 'bL',x)))
  #replace the conventional monitoring results
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('NF', 'NR',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('FB', 'PR',x)))
  df_MONIS4_09 <- as.data.frame(apply(df_MONIS4_09,2,function(x)gsub('FM4', 'SR',x)))
  
  #exclude rows where row names match species that should be excluded
  df_MONIS4_09 <- df_MONIS4_09[!row.names(df_MONIS4_09)=="Paralithodes_camtschaticus", ]
  df_MONIS4_09 <- df_MONIS4_09[!row.names(df_MONIS4_09)=="Homarus_americanus", ]
  #methods  disagree, eDNA says no, traditional monitoring says yes
  eval.cat01 <- c("NoCt_FM4") # blue
  eval.cat01 <- c("NC_SR") # blue
  #methods almost disagree, eDNA says no, traditional monitoring says yes
  eval.cat02 <- c("AbLOD_FM4", "BeLOD_FB", "BeLOD_FM4", "NoCt_FB") # light blue
  eval.cat02 <- c("aLbL_SR", "bL_PR", "bL_SR", "NC_PR") # light blue
  #methods almost disagree, eDNA says yes, traditional monitoring says no
  eval.cat03 <- c("1AbLOQ_FB", "3AbLOQ_FB", "AbLOD_FB", "AbLOD_NF", "BeLOD_NF") # light red
  eval.cat03 <- c("1aL_PR", "3aL_PR", "aLbL_PR", "aLbL_NR", "bL_NR") # light red
  #methods disagree, eDNA says yes, traditional monitoring says no
  eval.cat04 <- c("1AbLOQ_NF", "3AbLOQ_NF") # red
  eval.cat04 <- c("1aL_NR", "3aL_NR") # red
  #methods agree with both methods being positive
  eval.cat05 <- c("1AbLOQ_FM4", "3AbLOQ_FM4") # lightgreen
  eval.cat05 <- c("1aL_SR", "3aL_SR","aLbL_PR","aLbL_SR") # lightgreen
  #methods agree with both methods being negative
  eval.cat06 <- c("NoCt_NF") # white
  eval.cat06 <- c("NC_NR") # white
  
  #copy the data frame
  sbs.MO2_df <- df_MONIS4_09
  
  #copy the data frame
  df_sbs.MO.01 <- sbs.MO2_df
  # sum up the count per column , but exclude counting the first row, as
  # this holds harbour abbreviations
  df_sbs.MO.01[nrow(df_sbs.MO.01)+1,] <- colSums(
                                                  df_sbs.MO.01[-1,]=="NC_NR" |
                                                  df_sbs.MO.01[-1,]=="3aL_SR" |
                                                  df_sbs.MO.01[-1,]=="1aL_SR"  )
  
  #get the number of rows
  nr_df_sbs <- length(row.names(df_sbs.MO.01))
  #get the last row in the df, and get the row name, and change the row name
  row.names(df_sbs.MO.01)[nr_df_sbs] <- "total agreements"
  
  sbs.MO2_df <- df_sbs.MO.01
  #place the data frame in a tableHTML object
  tableHTML <- sbs.MO2_df %>% 
    tableHTML()
  # assign the eval category to a variable
  words <- eval.cat01 #  blue category
  col.f.cell <- "blue" # use this color for the cell
  col.f.font <- "white" #use this color for the font
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
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
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "contains",
                                 value = word,
                                 css = list(c("background-color"),
                                            c(col.f.cell)))
  }
  # assign the eval category to a variable
  words <- eval.cat05 #green
  # color the cells if it is a match
  col.f.cell <- "lightgreen" #light green
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "contains",
                                 value = word,
                                 css = list(c("background-color"),
                                            c(col.f.cell)))
  }
  
  # assign the eval category to a variable
  words <- eval.cat06 #whte
  # color the cells if it is a match
  col.f.cell <- "white" #white category
  for (word in words) {
    tableHTML <- tableHTML %>% 
      add_css_conditional_column(columns = 1:l.s.MO, #make it work on column 2 to the last column
                                 conditional = "contains",
                                 value = word,
                                 css = list(c("background-color"),
                                            c(col.f.cell)))
  }
  
  t.HTML17 <- tableHTML
  #and to export in a file
  write_tableHTML(t.HTML17, file = paste(wd00,wd10,"/","suppmatr_10.14d_App_G_table_max_eDNA_vs_convmnt.html", sep=""))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #
  #
  #