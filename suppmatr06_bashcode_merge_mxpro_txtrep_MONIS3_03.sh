#!/usr/bin/env bash
# -*- coding: utf-8 -*-
echo $BASH_VERSION

##get the present directory
WD=$(pwd)

#INDIR1="MONIS3_qpcrruns_20180515_w_txt_rep_data"
#INDIR1="MONIS3_qpcr_results_assembl_2018jun26_05"
INDIR1="suppmatr05_raw_qPCRdata"
INDIR2="suppmatr05b_input01_mxpro_files_to_merge"

OUTDIR1="suppmatr08_merged_qPCRdata_txt_reports"
#OUTDIR1="out01_merged_txtfiles_from_mxpro"
OUTFILE1="outfile01_merged_txtrepfiles_from_mxpro.csv"

#remove the old versions of the in- and output directory
rm -rf ${OUTDIR1}
rm -rf ${INDIR2}
#make new versions of the in- and output directory
mkdir ${OUTDIR1}
mkdir ${INDIR2}

#change directory to the directory w raw input files
cd ${INDIR1}
#copy these files to the new directory
cp *.txt "${WD}"/"${INDIR2}"/
#change directory to new directory w copied input files
cd "${WD}"/"${INDIR2}"/

#loop over all txt-files and modify the filename-endings, and move the resulting files
for FILENAME in *.txt
do
	NEWFILENAME=$(echo "${FILENAME}" | sed 's/ /_/g' | sed 's/_-_Text_Report_Data//g' | sed 's/\#/no/g')
	mv "${FILENAME}" "${NEWFILENAME}"
done

#loop over all txt-files, and replace various parts inside the file, and write the resulting text to a csv-file
TXT_FILEs=$(ls *.txt | sed s/.txt//g)
for FILE in ${TXT_FILEs}
do
	cat ${FILE}.txt | \
	# see this weblink for how to ignore foreign characters:	#https://stackoverflow.com/questions/19242275/re-error-illegal-byte-sequence-on-mac-os-x/19770395#19770395
#	LC_ALL=C sed '/multiplex/d' | \ # delete lines with multiplex occuring
	#alter all occurences of 'not_used' to 'notused'
	LC_ALL=C sed 's/not_used/notused/g' | \
	#see how to remove tabs here: https://stackoverflow.com/questions/5398395/how-can-i-insert-a-tab-character-with-sed-on-os-x?noredirect=1&lq=1
	sed -E $'s/\t/;/g' | sed 's/ //g' > $FILE.csv
#	echo ${FILE}.csv
#	cat ${FILE}.txt | grep multi 
	#head -10 $FILE.csv
done


OUTFILE1_MIX_EFT="MONIS3_AssID21_Corcas_efteraar_qpcrrundate20180515_02.csv"
OUTFILE1_MIX_FOR="MONIS3_AssID21_Corcas_foraar_qpcrrundate20180515_02.csv"

#modify the files in a loop, the loop can be used if there are more incorrect qPCR run files
MIX_FOR_EFT_FILES="MONIS3_AssID21_Corcas_efteraar_qpcrrundate20180515_retest_07080910.csv"
for FILE in ${MIX_FOR_EFT_FILES}
do
	head -1 ${FILE} > ${OUTFILE1_MIX_EFT}
	grep efteraar ${FILE} | sed 's/efteraar//g' >> ${OUTFILE1_MIX_EFT}
	head -1 ${FILE} > ${OUTFILE1_MIX_FOR}
	grep foraar ${FILE} | sed 's/foraar//g' >> ${OUTFILE1_MIX_FOR}
done


INFILE1_MIX_FOR="MONIS3_AssID21_Corcas_foraar_qpcrrundate20180515.csv"
#cat everything, apart from the first line into the outfile
cat ${INFILE1_MIX_FOR} | sed '1d' >> ${OUTFILE1_MIX_FOR}

#move the original mixed qPCR run file
mv MONIS3_AssID21_Corcas_foraar_qpcrrundate20180515.csv MONIS3_AssID21_Corcas_foraar_qpcrrundate20180515_original.csv

#move the two outfiles , and give them the required filenames
mv ${OUTFILE1_MIX_FOR} MONIS3_AssID21_Corcas_foraar_qpcrrundate20180515.csv
mv ${OUTFILE1_MIX_EFT} MONIS3_AssID21_Corcas_efteraar_qpcrrundate20180515.csv


# get only the first part of the filename
CSV_FILENAMES=$(ls *.csv | sed s/.csv//g)

#loop over all csv-files and modify the contents, and end with making new csv-files
for ENDING in ${CSV_FILENAMES}
do
# for how to convert line endings in DOS-file
#see this website: https://stackoverflow.com/questions/2613800/how-to-convert-dos-windows-newline-crlf-to-unix-newline-lf-in-a-bash-script
	echo "inputfile format is:"
	file ${ENDING}.csv #check the file format of the input file 
	tr -d '\015' <${ENDING}.csv >${ENDING}01.csv # replace DOS CRLF-end-of-lines
	echo "outputfile format is:"
	file ${ENDING}01.csv #check the file format of the output file
# use sed command on every line except the first line, but instead add ";MONISprojectno_AssayID_season_qpcrrundate" to first line
	sed '1 s,$,;MONISprojectno_AssayID_speciesabbr_season_qpcrrundate,; 1! s,$,'';'${ENDING}',' ${ENDING}01.csv > "${ENDING}"02.csv
	#use sed to remove spaces in values
	sed 's/ //g' "${ENDING}"02.csv > "${ENDING}"03.csv
	#only print the last column in the file
	#awk -F ";" '{print $NF}' ${ENDING}03.csv
	#replace in a specified column, see: https://stackoverflow.com/questions/42004482/shell-script-replace-a-specified-column-with-sed
	# for some reason this replaces semicolons w spaces in the other columns - I do not know why
	awk -F";" '{gsub("_",";",$NF)}1' ${ENDING}03.csv | \
	#but with sed the spaces can be replaced back to semicolons
	sed 's/ /;/g' | \
	#and the remaining underscores can be replaced
	sed 's/_/;/g' | \
	#and the ;WellName; can be split into new column names and added semicolons
	#sed 's/;WellName;/;replno;specs;smpltp;/g' | \
	#use square brackets to allow sed to replace parentheses
	sed 's/[(]//g' | sed 's/[)]//g' | \
	#the csv-file had decimal numbers with commas instead of points, replace commas w points
	sed 's/,/./g' | \
	#use sed to replace double separators with single separators
	sed 's/;;/;/g' | \
	#delete line with 'NotinUse'
	awk '!/NotinUse/' | \
	#delete line with 'notused'
	awk '!/notused/' | \
	#delete line with '---;---;Unknown;0.1'
	awk '!/---;---;Unknown;0.1/' | \
	#delete line with 'retest' - to remove the rerun tests on Corcas 
	awk '!/retest/' | \
	#delete line with 'original' - to remove the rerun tests on Corcas 
	awk '!/original/' | \
	#delete line with 'Reference'
	awk '!/Reference/' > ${ENDING}06.csv 
done	

#uncomment part below if some of the individual input files have different columns
##in a loop check if the input .csv-files have the word 'Dye' included
#CSV04_FILENAMES=$(ls *04.csv | sed s/04.csv//g)
#
#for ENDING in ${CSV04_FILENAMES}
#do
#	## check if the input file has the word 'Dye' included
#if grep -Fq Dye ${ENDING}04.csv; then
#	while IFS= read -r line
#	do
#		## Assuming the fifth column always holds the 'Dye' column
#		## With cut fields 1 to 4 and from 6 an onwards are retained : see this website https://www.cyberciti.biz/faq/unix-linux-bsd-appleosx-skip-fields-command/
#	     cut -d ';' -f1-4,6- <<<"$line"
#	    ### same stuff with awk ###
#	    ### awk '{print substr($0, index($0,$3))}' <<< "$line" ###
#	done < "${ENDING}"04.csv > ${ENDING}05.csv
#else NEWFILENAME=$(echo "${ENDING}05.csv")
#	cp "${ENDING}"04.csv "${NEWFILENAME}"
#fi
#done
#
#CSV05_FILENAMES=$(ls *05.csv | sed s/05.csv//g)
##in a loop check if the input .csv-files have the word 'Replicate' included
#for ENDING in ${CSV05_FILENAMES}
#do
#	## check if the input file has the word 'Replicate' included
#if grep -Fq Replicate ${ENDING}05.csv; then
#	while IFS= read -r line
#	do
#		## Assuming the sixth column always holds the 'Replicate' column
#		## With cut fields 1 to 5 and from 7 an onwards are retained : see this website https://www.cyberciti.biz/faq/unix-linux-bsd-appleosx-skip-fields-command/
#	     cut -d ';' -f1-5,7- <<<"$line"
#	    ### same stuff with awk ###
#	    ### awk '{print substr($0, index($0,$3))}' <<< "$line" ###
#	done < "${ENDING}"05.csv > ${ENDING}06.csv
#else NEWFILENAME=$(echo "${ENDING}06.csv")
#	cp "${ENDING}"05.csv "${NEWFILENAME}"
#fi
#done


#see note about quotes around variables: https://stackoverflow.com/questions/2462385/getting-an-ambiguous-redirect-error
# especially for writing to a file in a path that incl. spaces
for FILE in *06.csv
do
	#write the first line of every csv-file into a temporary file
	head -1 ${FILE} >> "${WD}"/"${OUTDIR1}"/tmp01.txt
done

#get the unique lines from the tmp01.txt file, 
#if all csv-files are set up in the same way, this should return only a single line
#this line can put into the outputfile and serve as a header with column names
cat "${WD}"/"${OUTDIR1}"/tmp01.txt | uniq > "${WD}"/"${OUTDIR1}"/"${OUTFILE1}"

#see this website on how to use sed to get all lines apart from the first line: https://unix.stackexchange.com/questions/55755/print-file-content-without-the-first-and-last-lines/55757
for FILE in *06.csv
do
	sed '1d' ${FILE} >> "${WD}"/"${OUTDIR1}"/"${OUTFILE1}"
done



#head -10 "${WD}"/"${OUTDIR1}"/"${OUTFILE1}"
#tail -10 "${WD}"/"${OUTDIR1}"/"${OUTFILE1}"

# see this website about : Echo newline in Bash prints literal \n
# https://stackoverflow.com/questions/8467424/echo-newline-in-bash-prints-literal-n
# -e flag did it for me, which "enables interpretation of backslash escapes"

echo -e " \n make sure there only is one unique line for headers \n"

#see the content of the tmp01.txt file, to check all input files have the same header
#using the uniq command in the end , will make sure it only returns the unique lines 
cat "${WD}"/"${OUTDIR1}"/tmp01.txt | uniq

echo -e " \n make sure there only is one unique species name per species \n"
#Print the third column
# and sort the output, and get only uniq values
awk -F";" '{print $3}' "${WD}"/"${OUTDIR1}"/"${OUTFILE1}" | sort | uniq

echo -e " \n make sure there only is one unique sample type name per sample type \n"
#Print the fourth column
# and sort the output, and get only uniq values
awk -F";" '{print $2}' "${WD}"/"${OUTDIR1}"/"${OUTFILE1}" | sort | uniq

printf "${WD}"/"${OUTDIR1}"/"${OUTFILE1}"

#delete all the temporary files
rm *01.csv
rm *02.csv
rm *03.csv
rm *04.csv
rm *05.csv
rm *06.csv
rm "${WD}"/"${OUTDIR1}"/tmp01.txt
