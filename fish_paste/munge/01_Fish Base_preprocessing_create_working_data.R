#load(file = "data/ALL_REA_FISH_RAW.rdata")
#site_master<-read.csv(file = "data/SITE MASTER.csv")
#sectors<-read.csv(file = "data/Sectors-Strata-Areas_UPD.csv")

#source("lib/Islandwide Mean_Variance Functions.r")
#source("lib/fish_team_functions.R")

sectors<-Sectors.Strata.Areas.UPD
site_master<-SITE.MASTER

levels(sectors$REGION)<-c(levels(sectors$REGION), "S.MARIAN", "N.MARIAN")
sectors[sectors$ISLAND %in% c("Guam", "Rota", "Aguijan",
                              "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
sectors[sectors$ISLAND %in% c("Alamagan","Guguan","Sarigan",
                              "Pagan", "Agrihan", "Asuncion", "Maug",
                              "Farallon de Pajaros",
                              "Sarigan-Guguan-Alamagan"),]$REGION<-"N.MARIAN"

sectors<-droplevels(sectors)

# FISH REA WORKINGS ------------------------------------------------------------
x<-df

# HOUSEKEEPING -----------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE",
             "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",
             "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
             "REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH",
             "HARD_CORAL", "MA", "TA",  "CCA",  "SAND",  "SOFT_CORAL",
             "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE",
             "ZOANTHID" , "COMPLEXITY", "SPECIES", "COUNT", "SIZE_",
             "OBS_TYPE", "SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20",
             "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100",
             "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT", "SCIENTIFIC_NAME",
             "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" ,
             "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",
             "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#generate a simple "Strata" field, by concatenating Stratum and Depth fields
##x$STRATA<-paste(x$REEF_ZONE, x$DEPTH_BIN, sep='')

## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$METHOD %in% c("BLT", "nSPC"), drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))

#add SEC_NAME to x
# this would be better if SECTOR field in database was up to date properly ..
## rather than merge with the site_Sectors spreadsheet
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC",
                           "ANALYSIS_YEAR", "ANALYSIS_STRATA")],
         by="SITE", all.x=TRUE)

#CHECK THAT all ANALYSIS_SEC are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SEC)  & x$METHOD=="nSPC",
       c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("nSPC sites with MISSING ANALYSIS_SEC")}   # should be 0

#for ones that are missing, set it to ISLAND
no_secs<-is.na(x$ANALYSIS_SEC)
tmp<-as.character(x$ANALYSIS_SEC)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$ANALYSIS_SEC<-tmp

# change format of DATE_ from character to date
#x$DATE_<-as.Date(x$DATE_)
# set year to factor rather than default interval
#x$OBS_YEAR<-as.factor(x$OBS_YEAR)


################################################################################
###### new section .. where there is substrate_height data, work out
## average height && ave_height_variability so that we get standardized
## complexity metrics (mean hieght, mean height variability, max-height)
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20",
                          "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100",
                          "SUBSTRATE_HEIGHT_150"))]
################################################################################

### tidy up ISLAND entries
## change ISLAND of Lehua to Niihau -Lehua is a rock slightly offshore of Niihau
x[x$ISLAND=="Lehua",]$ISLAND<-"Niihau"
x<-droplevels(x)

#convert COMPLEXITY to a numeric field ###
x$COMPLEXITY<-as.vector(toupper(x$COMPLEXITY))
x[is.na(x$COMPLEXITY),"COMPLEXITY"]<-"UNKNOWN"
COMPLEXITY_VALUES<-toupper(c("Low", "Med-Low", "Med",
                             "Med-Hi", "Hi", "Very-Hi"))
x$ComplexityValue<-NaN
for (i in 1:length(COMPLEXITY_VALUES)){
	if(COMPLEXITY_VALUES[i] %in% x$COMPLEXITY){
		x[x$COMPLEXITY==COMPLEXITY_VALUES[i],]$ComplexityValue<-i
	}
}

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
x[is.na(x$LATITUDE),]$LATITUDE<-0
x[is.na(x$LONGITUDE),]$LONGITUDE<-0

#temporary fix to ensure that a fish species "SAND" does not confuse the code when there is also a benthic category "SAND".
# both SAND and SNDP exist in the species_table and refer to the same taxa (Parapercis sp.)
# better long term fix is to change species codes of SAND into SNDP in the database
x[x$SPECIES=="SAND",]$SPECIES="SNDP"  

###x[is.na(x$LMAX),]$LMAX<-999

## separate out the north and south marianas
levels(x$REGION)<-c(levels(x$REGION), "S.MARIAN", "N.MARIAN")
x[x$ISLAND %in% c("Guam", "Rota", "Aguijan",
                  "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
x[x$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan",
                  "Agrihan", "Asuncion", "Maug",
                  "Farallon de Pajaros"),]$REGION<-"N.MARIAN"

x<-droplevels(x)

# WORKING WITH POOLING READY DATA FROM HERE ON  --------------------------------
################################################################################
### MAIN DATA FILTERING AND CLEANING IS DONE ..
### SHOULD BE READY TO START WORKING WITH THE DATA FROM HERE ON ###########

# we want to keep x as cleaned up data.. and only work with a
## 'working data' version of that. Calling that working data 'wd'
wd<-x

#check for NAs and **** FOR NOW **** set those to zero, so rows do not
## get dropped by the aggregate function (below)
## the error returned are ok to ignore, just refers to invalid factor levels
#wd[is.na(wd)]<-0

#base information about the survey - field names should match
## those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND",
               "ANALYSIS_SEC", "ANALYSIS_YEAR",
               "ANALYSIS_STRATA", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SOFT_CORAL",
                    "MA", "CCA", "TA", "SAND", "CYANO",
                    "OTHER_BENTHIC", "ComplexityValue",
                    "MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% ..
##I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE
# DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps
# have NAs in a particular BENTHIC_FIELDS, but other records have
#non-zeros - in that situation, we were recording a field but one other
#diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather
#than here (as its an error at time of data entry)
#### BELOW code does the job, but should be cleaned up and put in a function
### IDW- COULD GREATLY SPEED THIS UP BY DOING IT FOR A REGION AND YEAR
#.. AND LIMIT TO ONLY nSPC
# i.e make the checking for some data and some NAs at the levels of a
#full survey round .. and also use indixes into the wd structure,
# rather than create temp dfs (tmp_data)
BENTHIC_FIELDS<-c("HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND",
                  "CYANO", "CLAM", "CORALLIMORPH", "ZOANTHID",
                  "TUNICATE", "SPONGE")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(wd, UNIQUE_ROUND)

wd$countBD<-apply(wd[,BENTHIC_FIELDS], 1,
                  function(xx) length(which(!is.na(xx))))
#IDW 10-22-2013 checking for situation where there is NO benthic data at all
for(i in 1:dim(round_table)[1])
{
	if(round_table[i,"METHOD"]=="nSPC")
	{
		tmp_data<-wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"]
                             & wd$METHOD==round_table[i,"METHOD"]
                             & wd$REGION==round_table[i,"REGION"],]

		#go through BENTHIC_FIELDS, checking whether
                # there are some NAs and some data values
		for(j in 1:length(BENTHIC_FIELDS))
		{
			## IF there are both non NAs and NAs
			if(length(tmp_data[!is.na(
                                tmp_data[,BENTHIC_FIELDS[j]]),
                                           BENTHIC_FIELDS[j]]) > 0
			        & length(tmp_data[
                                        is.na(tmp_data[,BENTHIC_FIELDS[j]]),
                                        BENTHIC_FIELDS[j]]) > 0)
			{
				#set all NAs of that field to 0
				tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),
                                         BENTHIC_FIELDS[j]]<-0

				#now rewrite the benthic fields with NAs
                                #converted to zeros
				wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"]
                                   & wd$METHOD==round_table[i,"METHOD"]
                                   & wd$REGION==round_table[i,"REGION"],
                                   BENTHIC_FIELDS[j]]<-tmp_data[,
                                BENTHIC_FIELDS[j]]
			}
		}

	}
}
# now reset zeros to NAs for all records where there was NO benthic data at all
wd[wd$countBD==0,BENTHIC_FIELDS]<-NA

