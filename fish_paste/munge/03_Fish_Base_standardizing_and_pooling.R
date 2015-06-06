################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A
#SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
################################################################################

# STANDARDIZING DATA ------------------------------------------------------

## DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$ISLAND), unique(wsd$ISLAND))
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

# THESE ARE SPC only surveys, already removed
# drop surveys done at Stingray and Zealandia as these were one offs...
#x<-x[-c(which(x$ISLAND =="Stingray"),which(x$ISLAND =="Zealandia")),]

# IDW-SOME CLEAN UP
#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND'
# there MUST already be appropriate records in the sectors table for the new
# 'ISLAND' name, in this case will be "AGS"
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
levels(sectors$ISLAND)<-c(levels(sectors$ISLAND), "AGS")
wsd[wsd$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

#set all Backreef in NWHI, Samoa, and Maraians as single DEPTH_ZONE ("All")
wsd[wsd$REGION %in% c("NWHI", "SAMOA", "S.MARIAN", "N.MARIAN")
    & wsd$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"
wsd<-droplevels(wsd)

WSD_SAVED<-wsd
SECTORS_SAVED<-sectors
################################################################################
#     SET THE ANALYIS SCHEME (ie WHICH SECTORS ARE WE USING THIS TIME ..
# IS IT THE BASIC ONES, OR THE ONES THAT WE USED FOR GUAMM2011 SURVEYS
# OR WHATEVER)
#
#
#  BE AWARE THAT THIS NEXT STEP REQUIRES SOME MANUAL FIDDLING..
#  WHEN YOU RUN THE CODE YOU MUST DECIDE ON THE APPROPRAITE STRATIFICATION
# SCHEME. IN SEVERAL CASES IT WILL
#        BE NECESSARY TO RUN SEVERAL SCHEMES
# (eg MARIANA 2011, then MARIANA 2014, etc...)
# AND THEN MANUALLY PUT THE DATA TOGETHER INTO A MASTER OUTPUT
# (eg run all with RAMP_BASIC,
#        then run just Guam 2011 with MARIAN2011, then run Guam2014
# with MARIAN2014, and then pool the various data files
# (eg by cutting and pasting from MAR2011 output into the master etc..)
#
################################################################################
wsd<-WSD_SAVED
sectors<-SECTORS_SAVED

# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE
# AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

# THIS IS A CRITICAL STEP - SET THE ANALYSIS SCHEME HERE .. ALL STEPS BELOW
# WILL WORK OFF THIS SCHEME (THIS IS HOW ISLANDS ARE BROKEN DOWN INTO SECTORS
# Analysis Schemes are : "RAMP_BASIC", "MARI2011", "MARI2014", "AS_SANCTUARY"
CURRENT_SCHEME<-"RAMP_BASIC";
#CURRENT_SCHEME<-"MARI2011"
#CURRENT_SCHEME<-"MARI2014"
#CURRENT_SCHEME<-"AS_SANCTUARY"
sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]


SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_STRATA")
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")

##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS
analysis_secs<-unique(wsd$ANALYSIS_SEC)
missing_secs<-unique(analysis_secs
                     [!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
if(length(missing_secs)>0) {
        cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
}
tmp<-Aggregate_InputTable(wd,
                          c("REGION", "ISLAND",
                            "ANALYSIS_YEAR", "ANALYSIS_SEC"))
tmp[tmp$ANALYSIS_SEC %in% missing_secs,]
#now deal with those missing sectors - either rename ANALYSIS_SEC OR remove
if(CURRENT_SCHEME=="RAMP_BASIC") {
        #in this case removing 2014 ACHANG_MPA sites (The shorebased ones)
        # and changing ANALYSIS_SEC for all other GUAM MPA sectors
        # to the RAMP base one "GUAM_MP"
        wsd<-wsd[!(wsd$ANALYSIS_SEC == "ACHANG_MPA" & wsd$ANALYSIS_YEAR==2014),]

        changerows<-wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA",
        "ACHANG_MPA", "TUMON_BAY_MPA",
        "PITI_BOMB_MPA",
        "GUAM_MP_MINUS_ACHANG")
        wsd$ANALYSIS_SEC[changerows]<-"GUAM_MP"
}
if(CURRENT_SCHEME=="MARI2011") {wsd<-wsd[(
        wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2011),]}
#in this case remove everything that isnt MARIANA surveyed in 2011
if(CURRENT_SCHEME=="MARI2014") {wsd<-wsd[(
        wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2014),]}
#in this case remove everything that isnt MARIANA surveyed in 2014
if(CURRENT_SCHEME=="AS_SANCTUARY") {wsd<-wsd[(
        wd$REGION == "SAMOA" & wsd$OBS_YEAR==2015),]}
#in this case remove everything that isnt SAMOA surveyed in 2015

### LOOK AT REPLICATION WITHIN STRATA - TO EYEBALL WHETHER THERE ARE
## STRATA WITHOUT REPLICATION
tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
tmp<-merge(sectors, tmp[,c("ANALYSIS_YEAR", "ANALYSIS_SEC",
                           "ANALYSIS_STRATA","x")],
           by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
names(tmp)[names(tmp)=="x"]<-"n_sites"
cast(tmp, ANALYSIS_YEAR + REGION + ISLAND +
             ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
#cast(tmp, REGION + ISLAND + SEC_NAME ~ REEF_ZONE + DEPTH_BIN,
# value="AREA_HA", sum, fill=NA)

#clean up the sectors table so pool all sub sectors within a scheme
#into a total for this scheme's sectors
sectors<-aggregate(sectors[,"AREA_HA"],
                   by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
names(sectors)[names(sectors)=="x"]<-"AREA_HA"

################################################################################
## NOW DO THE CALCULATION OF WINHIN-STRATA AND POOLED UP DATA VALUES ###########
################################################################################
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR", "METHOD")
# additional fields that we want to break data at, but which do not
# relate to physical areas (eg survey year or method)
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
write.csv(data.per.strata,file=paste(CURRENT_SCHEME,
                                     "tmp strata data.csv", sep=""))

###### REMOVE STRATA with N=1 (cannot pool those up)
data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
data.per.strata$SampleVar<-data.per.strata$SampleVar[
        data.per.strata$SampleVar$N>1,]
data.per.strata$SampleSE<-data.per.strata$SampleSE[
        data.per.strata$SampleSE$N>1,]


#pool up that strata level data to desired output level .. using the sectors
#df to determine appropriate weighting of each strata within each output level
AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to
#agggregate output data to (eg per REGION or per (REGION, ISLAND) etc...
data_pooled_is_yr<-Calc_Pooled(data.per.strata$Mean,
                               data.per.strata$SampleVar,
                               data.cols, AGGREGATION_LEVEL,
                               ADDITIONAL_POOLING_BY,
                               SPATIAL_POOLING_BASE, sectors)
#write.csv(data_pooled_is_yr,file=paste(CURRENT_SCHEME,
## "data_pooled_is_yr.csv", sep=""))
save(data_pooled_is_yr, file=paste(CURRENT_SCHEME,
                                   "data_pooled_is_yr.rdata", sep=""))

#pool up that strata level data to desired output level ..
# using the sectors df to determine appropriate weighting of each strata within
# each output level
## note: adding a new level of region to separate out the
## north and south marianas
ADDITIONAL_POOLING_BY<-c("METHOD")
AGGREGATION_LEVEL<-c("REGION","ISLAND")
# Spatial Level to agggregate output data to
#(eg per REGION or per (REGION, ISLAND) etc...
data_pooled_is<-Calc_Pooled(data.per.strata$Mean,
                            data.per.strata$SampleVar,
                            data.cols, AGGREGATION_LEVEL,
                            ADDITIONAL_POOLING_BY,
                            SPATIAL_POOLING_BASE, sectors)
#write.csv(data_pooled_is,file=paste(
#        CURRENT_SCHEME, "data_pooled_is.csv", sep=""))
save(data_pooled_is, file=paste(CURRENT_SCHEME, "data_pooled_is.rdata", sep=""))

#pool up that strata level data to desired output level ..
#using the sectors df to determine appropriate weighting of
#each strata within each output level
ADDITIONAL_POOLING_BY<-c("METHOD")
AGGREGATION_LEVEL<-c("REGION")
# Spatial Level to agggregate output data to (eg per REGION or per
#(REGION, ISLAND) etc...
data_pooled_rg<-Calc_Pooled(data.per.strata$Mean,
                            data.per.strata$SampleVar,
                            data.cols, AGGREGATION_LEVEL,
                            ADDITIONAL_POOLING_BY,
                            SPATIAL_POOLING_BASE, sectors)
#write.csv(data_pooled_rg,file=paste(CURRENT_SCHEME, "data_pooled_rg.csv", sep=""))
save(data_pooled_rg, file=paste(CURRENT_SCHEME, "data_pooled_rg.rdata", sep=""))



#******** END FISH REA DATA WORKINGS ******
#******************************************