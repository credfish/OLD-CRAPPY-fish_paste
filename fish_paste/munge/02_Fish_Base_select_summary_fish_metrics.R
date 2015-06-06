#********************
survey_est_benthos<-Calc_Site_nSurveysArea(wd,
        UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)
#Calc_Site_nSurveysArea deals better with situations
# where one REP has benthic data and other doesnt.

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY",
                       "COMMONFAMILYALL", "TROPHIC_MONREP",
                       "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

# SELECT SUMMARY METRICS --------------------------------------------------

# calc pooled site biomass by consumer group, species, and common family
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP");
        trophic.cols<-levels(species_table$TROPHIC_MONREP)
#r2a<-Calc_Site_Abund(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r2b<-Calc_Site_Bio(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r3<-Calc_Site_Bio(wd, "COMMONFAMILYALL"); family.cols<-levels(species_table$COMMONFAMILYALL)
r4b<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf));
        size.cols<-names(r4b)[3:dim(r4b)[2]]
#r5<-Calc_Site_Species_Richness(wd)

#Merge Site Data and Count Data Per Site Per Grouping Variable
# (e.g. Species, Tropic_MonRep, Family)
xx<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
wsd<-merge(xx,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
data.cols<-c(trophic.cols, "TotFish", SURVEY_SITE_DATA)
wsd<-merge(wsd, r4b, by=UNIQUE_SURVEY)
data.cols<-c(data.cols, "0_20", "20_50", "50_plus")
#need to make sure the cols nums are correct

names(wsd)
#names(wsd)[35:37]<-c("0_20", "20_50", "50_plus")
names(wsd)[match(c("[0,20]", "(20,50]","(50,Inf]" ),
                 names(wsd))] <- c("0_20", "20_50", "50_plus")
names(wsd)
wsd$BSR<-(wsd$HARD_CORAL+wsd$CCA)/(wsd$MA + wsd$TA)
##
data.cols<-c(data.cols, "BSR")

cache("wsd")

