# Food web calculations 2018
###########
### ipmortant addition ###
year <- 2018

# Preparation Calculation -----------------------------------------------------------------
area_IVc_km2 <- 63633.85614 #km2

# Using CPUE by age to calculate ratio greater NS-IVc and estimate Biomass in next step ----------------------------------------------------
# load CPUE csv file (2020-12-08)
NS_CPUE_all_years <- read.csv("data/CPUE per age per subarea_2020-12-08.csv", header = TRUE, stringsAsFactors = FALSE)
plaice_stock_numbers <- read.csv("data/plaice_stock_numbers_by_age.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1)
plaice_stock_numbers <- plaice_stock_numbers*1000 # as number in thousand
plaice_stock_weight <- read.csv("data/plaice_stock_weight_by_age.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1) #weigth in kg
colnames(plaice_stock_numbers) <- c(1:10)
colnames(plaice_stock_weight) <- c(1:10)

# define the areas
NS_CPUE_all_years$rowICES <- as.numeric(substr(NS_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area
NS_CPUE_all_years <- subset(NS_CPUE_all_years, Species != 'Melanogrammus aeglefinus') # haddock not a FG anymore

area_EC_7d <- subset(NS_CPUE_all_years,rowICES < 31)
area_IVc <- subset(NS_CPUE_all_years,rowICES > 30 & rowICES < 36)
area_NS <- subset(NS_CPUE_all_years,rowICES > 30 & rowICES < 52 & SubArea!="47G0" & SubArea!="43F9" & SubArea!="44F8" & SubArea!="44F9" & SubArea!="44G1" & SubArea!="45F8" & SubArea!="45F9" & SubArea!="45G1" & SubArea!="46F9" & SubArea!="46G1")
area_Ska_3a_20 <- subset(NS_CPUE_all_years, SubArea=="44F7"|SubArea=="43F8"|SubArea=="47G0"|SubArea=="43F9"|SubArea=="44F8"|SubArea=="44F9"|SubArea=="44G1"|SubArea=="45F8"|SubArea=="45F9"|SubArea=="45G1"
                         |SubArea=="46F9"|SubArea=="46G1")

ratio_of_biomass <- array(NA, dim =c(6,28), list(c("Gadus morhua", "Clupea harengus","Merlangius merlangus","Scomber scombrus","Sprattus sprattus","Pleuronectes platessa"),c(1991:2018))) # use scientific names as in CPUE file
plaice_TSB_area_IVc <- array(NA, dim =c(1,28),list("plaice TSB",c(1991:2018)))
for (year in c(1991:2018)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 2018
  area_EC_7d_year <- subset(area_EC_7d,area_EC_7d$Year == year) #33331.7614643 km2
  area_IVc_year <- subset(area_IVc,area_IVc$Year == year) #63633.8561355 km2
  area_NS_year <- subset(area_NS,area_NS$Year == year) # 611927.2 km2
  area_Ska_3a_20_year <- subset(area_Ska_3a_20,area_Ska_3a_20$Year == year) #34041.3688141 km2

  # calculate CPUE per age
  numb_IVc <- array(NA, dim =c(1,11,length(unique(area_IVc_year$Species))), list(1,c(0:10),unique(area_IVc_year$Species)))
  for (s in unique(area_IVc_year$Species)) {
    group <- subset(area_IVc_year, Species == s)[,c(8:18)]
    numb_IVc[,c(1:11),s] <- apply(group, 2, sum)

  }
  numb_IVc[is.na(numb_IVc)|is.nan(numb_IVc)|is.infinite(numb_IVc)] <- 0

  numb_total_area <- array(NA, dim =c(1,11,length(unique(area_IVc_year$Species))), list(1,c(0:10),unique(area_IVc_year$Species)))
  group <- subset(rbind(area_NS_year,area_Ska_3a_20_year), Species == "Pleuronectes platessa")[,c(8:18)]
  numb_total_area[,c(1:11),"Pleuronectes platessa"] <- apply(group, 2, sum)
  group <- subset(rbind(area_NS_year,area_EC_7d_year,area_Ska_3a_20_year), Species == "Gadus morhua")[,c(8:18)]   # use landings ratio instead
  numb_total_area[,c(1:11),"Gadus morhua"] <- apply(group, 2, sum)
  group <- subset(area_NS_year, Species == "Clupea harengus" )[,c(8:18)]
  numb_total_area[,c(1:11),"Clupea harengus"] <- apply(group, 2, sum)
  group <- subset(rbind(area_NS_year,area_Ska_3a_20_year), Species == "Sprattus sprattus" )[,c(8:18)]
  numb_total_area[,c(1:11),"Sprattus sprattus"] <- apply(group, 2, sum)
  group <- subset(rbind(area_NS_year,area_EC_7d_year), Species == "Merlangius merlangus")[,c(8:18)]
  numb_total_area[,c(1:11),"Merlangius merlangus"] <- apply(group, 2, sum)
  group <- subset(rbind(area_NS_year), Species == "Scomber scombrus")[,c(8:18)]
  numb_total_area[,c(1:11),"Scomber scombrus"] <- apply(group, 2, sum)

  numb_total_area[is.na(numb_total_area)|is.nan(numb_total_area)|is.infinite(numb_total_area)] <- 0


  # ratio IVc/total area
  ratio_IVc_total_area <- array(NA, dim =c(1,11,length(unique(area_IVc$Species))), list(1,c(0:10),unique(area_IVc$Species)))
  for (species in unique(area_IVc$Species)){
    for (col in c(1:11)){
      ratio_IVc_total_area[,col,species] <- numb_IVc[,col,species]/numb_total_area[,col,species]
      if (species == 'Sprattus sprattus'|species == 'Scomber scombrus'){ # TSB
        ratio_of_biomass[species,as.character(year)] <- sum(numb_IVc[,,species])/sum(numb_total_area[,,species])
      } else {
        if (species == 'Clupea harengus'){ #SSB (adult from age 1 = 12months)
          ratio_of_biomass[species,as.character(year)] <- sum(numb_IVc[,c(2:11),species])/sum(numb_total_area[,c(2:11),species])
        }else{ # SSB (adult from age 2 = 24months)
          ratio_of_biomass[species,as.character(year)] <- sum(numb_IVc[,c(3:11),species])/sum(numb_total_area[,c(3:11),species])

        }
      }
    }
  }
  ratio_IVc_total_area[is.na(ratio_IVc_total_area)|is.nan(ratio_IVc_total_area)|is.infinite(ratio_IVc_total_area)] <- 0

  #calculate plaice TSB in area IVc for later purpose (estimating the TSB for other flatfish)
  plaice_TSB_ratio <- numb_IVc[,c(2:11),"Pleuronectes platessa"]/numb_total_area[,c(2:11),"Pleuronectes platessa"]
  plaice_TSB_ratio[is.na(plaice_TSB_ratio)|is.nan(plaice_TSB_ratio)|is.infinite(plaice_TSB_ratio)] <- 0
  plaice_TSB_area_IVc[,as.character(year)] <- sum(plaice_TSB_ratio * plaice_stock_numbers[as.character(year),] * plaice_stock_weight[as.character(year),] / 1000) # TSB in tonnes

}
# rename rownames (species) to common name of ratio_of_biomass
rownames(ratio_of_biomass) <- c("Cod (adult)","Herring (adult)","Whiting (adult)","Mackerel","Sprat","Plaice (adult)") # rename biomass columns
# ratio_of_biomass['Whiting (adult)',"2018"] <- mean(ratio_of_biomass['Whiting (adult)',c(24:28)]) #skip this step for 2018 (was required for 1991)


#cod NOT Historical landingsratio -> better estimate
#  2018 landings data (will be used for ratio of pelagic fish)
landings_2018 <- read.csv("data/FDI Landings EU 2018.csv", header = TRUE, stringsAsFactors = FALSE)

landing_cod_IV <- subset(landings_2018, Species == 'COD')
landing_cod_IV <- subset(landing_cod_IV, Sub.region == "27.4.A"|Sub.region == "27.4.B"|Sub.region == "27.4.C"|Sub.region == "27.7.D"|Sub.region == "27.3.A")
landing_cod_IVc <- subset(landing_cod_IV,Sub.region == "27.4.C")
sum_landing_cod_IV <- rep(NA,64)
sum_landing_cod_IVc <- rep(NA,64)
for (i in c(18)){      # create warnings message about introducing NAs but it's okay as they are removed
  sum_landing_cod_IV[i] <- sum(na.omit(as.numeric(landing_cod_IV[,i])))
  sum_landing_cod_IVc[i] <- sum(na.omit(as.numeric(landing_cod_IVc[,i])))
}
sum_landing_cod_IV <- na.omit(sum_landing_cod_IV)
sum_landing_cod_IVc <- na.omit(sum_landing_cod_IVc)
ratio_landingIVc_IV <- sum_landing_cod_IVc/sum_landing_cod_IV
#### geskipt: ratio_of_biomass["Cod (adult)","2018"] <- mean(ratio_landingIVc_IV[1:10]) # 10-year mean from 1986-1995
ratio_of_biomass["Cod (adult)","2018"] <- ratio_landingIVc_IV ## alternatief met enkel 2018

# calculate biomass for 1991-2018
biomass_fish <- read.csv("data/Biomass_fish.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1) #load biomass csv file
colnames(biomass_fish) <- c("Herring (adult)","Sandeel","Sprat","Cod (adult)","Plaice (adult)","Sole (adult)","Turbot","Whiting (adult)","Mackerel") # rename biomass columns
biomass_fish <- as.data.frame(t(biomass_fish))
####### biomass_fish["Sprat",] <- biomass_fish["Sprat",]+(0.11*biomass_fish["Sprat",]) # Non-SSB is 11% of SSB -> TSB (estimation based on weigth juvenile vs weigt adult)
# correct NEA Mackerel biomass for North Sea stock
mackerel_NS_portion <- read.csv("data/Mackerel NS portion (WKPELA 2014).csv", header = TRUE, stringsAsFactors = FALSE) #load biomass csv file
colnames(mackerel_NS_portion) <- c(1991:2018)
biomass_fish["Mackerel",] <- (biomass_fish["Mackerel",] * mackerel_NS_portion[,as.character(c(1991:2018))]) / 2 # divide by 2 because only 6 months in NS during winter/autumn (Jansen (2014))

biomass_fish["Whiting (adult)",c(1:5)] <- (biomass_fish["Whiting (adult)",c(1:5)]-(biomass_fish["Whiting (adult)",c(1:5)]*0.08109553)) # correction on biomass for IBTS not trawling in VIId

Biomass_IVc_fish <- as.data.frame(array(NA,c(nrow(ratio_of_biomass),ncol(ratio_of_biomass)), list(c(rownames(ratio_of_biomass)),c(colnames(ratio_of_biomass)))))
for (species in rownames(ratio_of_biomass)){
  Biomass_IVc_fish[species,] <- ratio_of_biomass[species,]*biomass_fish[species,]
}


# Sole ------------------------------------------------------------------
# load csv files
Sole_plaice_CPUE_all_years <- read.csv("data/plaice and sole CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)
Sole_CPUE_all_years <- subset(Sole_plaice_CPUE_all_years,Sole_plaice_CPUE_all_years$Species == "Solea solea")

# define the areas
Sole_CPUE_all_years$rowICES <- as.numeric(substr(Sole_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area

# only interested in sole (adult) for ssb
sole_area_IVc <- subset(Sole_CPUE_all_years,rowICES > 30 & rowICES < 36 & LngtClass > 209) # Length maturity (Fishbase) is 21.3 cm, so equal or bigger then 210 mm is sole (adult)
sole_area_NS <- subset(Sole_CPUE_all_years,rowICES > 30 & rowICES < 52 & SubArea!="47G0"& SubArea!="43F9" & SubArea!="44F8"
                       & SubArea!="44F9" & SubArea!="44G1" & SubArea!="45F8" & SubArea!="45F9" & SubArea!="45G1"
                       & SubArea!="46F9" & SubArea!="46G1" & LngtClass > 209) # Length maturity (Fishbase) is 21.3, so equal or bigger then 210 mm is sole (adult)

bio_sole <- array(NA, dim =c(1,28), list("Sole (adult)",c(1991:2018)))
for (year in c(1991:2018)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 2018
  sole_area_IVc_year <- subset(sole_area_IVc,sole_area_IVc$Year == year) #63633.8561355 km2
  sole_area_NS_year <- subset(sole_area_NS,sole_area_NS$Year == year) # 611927.2 km2

  # changing NA with 0 for calculation purpose
  #sole_area_IVc_year[is.na(sole_area_IVc_year)] <- 0

  # calculate CPUE per quarter per age
  group <- sole_area_IVc_year[,"CPUE_number_per_hour"]
  numb_IVc <- sum(group)

  group <- sole_area_NS_year[,"CPUE_number_per_hour"]
  numb_NS <- sum(group)

  # ratio IVc/NS
  ratio_IVcNS <- numb_IVc/numb_NS

  # multiply ratio with ssb
  bio_sole["Sole (adult)",as.character(year)] <- ratio_IVcNS * biomass_fish["Sole (adult)",as.character(year)]
}
Biomass_IVc_fish["Sole (adult)",] <- bio_sole["Sole (adult)",]

# other_flatfish & dab & rays ------------------------------------------------------------------
# load csv files
other_flatfish_CPUE_all_years <- read.csv("data/other_flatfish CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)
dab_CPUE_all_years <- read.csv("data/dab CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)
rays_CPUE_all_years <- read.csv("data/rays CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)
Sole_plaice_CPUE_all_years <- read.csv("data/plaice and sole CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)
Plaice_CPUE_all_years <- subset(Sole_plaice_CPUE_all_years,Sole_plaice_CPUE_all_years$Species == "Pleuronectes platessa")

# define the areas
Plaice_CPUE_all_years$rowICES <- as.numeric(substr(Plaice_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area
other_flatfish_CPUE_all_years$rowICES <- as.numeric(substr(other_flatfish_CPUE_all_years$SubArea,1,2))
dab_CPUE_all_years$rowICES <- as.numeric(substr(dab_CPUE_all_years$SubArea,1,2))
rays_CPUE_all_years$rowICES <- as.numeric(substr(rays_CPUE_all_years$SubArea,1,2))

# using Sparholt (1990) method using a reference to estimate abundance/biomass
plaice_area_IVc <- subset(Plaice_CPUE_all_years,rowICES > 30 & rowICES < 36)

other_flattfish_area_IVc <- subset(other_flatfish_CPUE_all_years,rowICES > 30 & rowICES < 36)
other_flattfish_area_NS <- subset(other_flatfish_CPUE_all_years,rowICES > 30 & rowICES < 52 & SubArea!="47G0" & SubArea!="43F9" & SubArea!="44F8"
                                  & SubArea!="44F9" & SubArea!="44G1" & SubArea!="45F8"& SubArea!="45F9" & SubArea!="45G1"
                                  & SubArea!="46F9" & SubArea!="46G1")

dab_area_IVc <- subset(dab_CPUE_all_years,rowICES > 30 & rowICES < 36)
rays_area_IVc <- subset(rays_CPUE_all_years,rowICES > 30 & rowICES < 36)


bio_other_flatfish <- array(NA, dim =c(1,28), list("other_flatfish",c(1991:2018)))
bio_dab <- array(NA, dim =c(1,28), list("dab",c(1991:2018)))
bio_rays <- array(NA, dim =c(1,28), list("rays",c(1991:2018)))
for (year in c(1991:2018)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 2018
  plaice_area_IVc_year <- subset(plaice_area_IVc,Year == year)
  other_flatfish_area_IVc_year <- subset(other_flattfish_area_IVc,Year == year)
  other_flatfish_area_NS_year <- subset(other_flattfish_area_NS,Year == year)
  dab_area_IVc_year <- subset(dab_area_IVc,Year == year)
  rays_area_IVc_year <- subset(rays_area_IVc,Year == year)

  #calculate IVcNS ratio for other flatfish - plaice & dab - plaice & rays - plaice using estimated TSB earlier from stock weight and numbers
  ratio_IVcNS <- sum(other_flatfish_area_IVc_year$CPUE_number_per_hour)/sum(plaice_area_IVc_year$CPUE_number_per_hour)
  bio_other_flatfish[, as.character(year)] <- ratio_IVcNS * plaice_TSB_area_IVc[, as.character(year)]
  ratio_IVcNS_dab <- sum(dab_area_IVc_year$CPUE_number_per_hour)/sum(plaice_area_IVc_year$CPUE_number_per_hour)
  bio_dab[, as.character(year)] <- ratio_IVcNS_dab *plaice_TSB_area_IVc[, as.character(year)]
  ratio_IVcNS_rays <- sum(rays_area_IVc_year$CPUE_number_per_hour)/sum(plaice_area_IVc_year$CPUE_number_per_hour)
  bio_rays[, as.character(year)] <- ratio_IVcNS_rays * plaice_TSB_area_IVc[, as.character(year)]
}
Biomass_IVc_fish["Other flatfish",] <- bio_other_flatfish
Biomass_IVc_fish["Dab",] <- bio_dab
Biomass_IVc_fish["Rays",] <- bio_rays

# sharks, other gadoids, demersal, sea bass, sandeel, horse mackerel & pelagic  ------------------------------------------------------------------
# load csv files
shark_et_al_CPUE_all_years <- read.csv("data/shark et al CPUE per length per subarea_2020-12-16.csv", header = TRUE, stringsAsFactors = FALSE)
demersal_CPUE_all_years <- read.csv("data/demersal CPUE per length per subarea_2020-12-16.csv", header = TRUE, stringsAsFactors = FALSE)

# define the areas
shark_et_al_CPUE_all_years$rowICES <- as.numeric(substr(shark_et_al_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area
demersal_CPUE_all_years$rowICES <- as.numeric(substr(demersal_CPUE_all_years$SubArea,1,2))

# using Sparholt (1990) method using a reference to estimate abundance/biomass
shark_et_al_area_IVc <- subset(shark_et_al_CPUE_all_years,rowICES > 30 & rowICES < 36)
demersal_area_IVc <- subset(demersal_CPUE_all_years,rowICES > 30 & rowICES < 36)
sandeel_area_1r <-  rbind(shark_et_al_area_IVc,subset(shark_et_al_CPUE_all_years, SubArea == "36E9" | SubArea =="36F0" | SubArea =="36F1" | SubArea =="36F2"| SubArea =="36F3"| SubArea =="36F4"| SubArea =="36F5"| SubArea =="36F6"| SubArea =="37E9"
                                                      | SubArea =="37F0"| SubArea =="37F1"| SubArea =="37F2"| SubArea =="37F3"| SubArea =="37F4"| SubArea =="37F5"| SubArea =="37F6"| SubArea =="38F0"| SubArea =="38F1"| SubArea =="38F2"
                                                      | SubArea =="38F3"| SubArea =="38F4"| SubArea =="38F5"| SubArea =="39F0"| SubArea =="39F1"| SubArea =="39F2"| SubArea =="39F3"| SubArea =="39F4"| SubArea =="39F5"| SubArea =="40F0"
                                                      | SubArea =="40F1"| SubArea =="40F2"| SubArea =="40F3"| SubArea =="40F4"| SubArea =="40F5"| SubArea =="41F4"| SubArea =="41F5"))


bio_sharks_et_al <- array(NA, dim =c(7,28), list(c("Sharks","Other gadoids","Pelagic","Sea bass", "Horse mackerel","Sandeels","Demersals"),c(1991:2018)))

for (year in c(1991:2018)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 2018
  shark_et_al_area_IVc_year <- subset(shark_et_al_area_IVc,Year == year)
  demersal_area_IVc_year <- subset(demersal_area_IVc,Year == year)
  if (year < 1999){
    sandeel_area_1r_year <-  subset(sandeel_area_1r,sandeel_area_1r$Year == year  & Species=="Ammodytidae")
  } else {
    sandeel_area_1r_year <-  subset(sandeel_area_1r,sandeel_area_1r$Year == "1999"  & Species=="Ammodytidae")  # ○nly data until 1999
  }

  # subset whiting, cod, herring & sprat separately AND other by FG separately
  cod_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Gadus morhua")
  other_gadoids_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Enchelyopus cimbrius" |Species== "Ciliata mustela" |Species== "Gaidropsarus
vulgaris" |Species== "Trisopterus minutus" |Species== "Trisopterus luscus" |Species== "Gadiculus argenteus")
  pelagic_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species =="Alosa"|Species == "Engraulis encrasicolus"|Species == "Sardina pilchardus")
  sea_bass_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Dicentrarchus labrax")
  horse_mackerel_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Trachurus trachurus")
  whiting_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Merlangius merlangus")
  herring_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Clupea harengus")
  sprat_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Sprattus sprattus")
  sharks_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Squalus acanthias"|Species== "Galeorhinus galeus"|Species== "Scyliorhinus canicula"|Species== "Mustelus")
  if (year < 1999){
    sandeel_area_IVc_year <- subset(shark_et_al_area_IVc_year,Species=="Ammodytidae")
  } else {
    sandeel_area_IVc_year <- subset(shark_et_al_area_IVc,Year == "1999"  & Species=="Ammodytidae")
  }

  #calculate IVcNS ratio for sharks - cod , other gadoids - cod , demersal - cod , pelagic - herring , sea bass - cod/whiting & horse mackerel - whiting using estimated TSB earlier from stock weight and numbers
  ratio_IVcNS <- sum(sharks_area_IVc_year$CPUE_number_per_hour)/(sum(cod_area_IVc_year$CPUE_number_per_hour) + sum(whiting_area_IVc_year$CPUE_number_per_hour))
  bio_sharks_et_al["Sharks", as.character(year)] <- ratio_IVcNS * (Biomass_IVc_fish["Cod (adult)", as.character(year)] + Biomass_IVc_fish["Whiting (adult)", as.character(year)])
  ratio_IVcNS <- sum(other_gadoids_area_IVc_year$CPUE_number_per_hour)/(sum(cod_area_IVc_year$CPUE_number_per_hour) + sum(whiting_area_IVc_year$CPUE_number_per_hour))
  bio_sharks_et_al["Other gadoids", as.character(year)] <- ratio_IVcNS * (Biomass_IVc_fish["Cod (adult)", as.character(year)] + Biomass_IVc_fish["Whiting (adult)", as.character(year)])
  ratio_IVcNS <- sum(demersal_area_IVc_year$CPUE_number_per_hour)/(sum(cod_area_IVc_year$CPUE_number_per_hour) + sum(whiting_area_IVc_year$CPUE_number_per_hour))
  bio_sharks_et_al["Demersals", as.character(year)] <- ratio_IVcNS * (Biomass_IVc_fish["Cod (adult)", as.character(year)] + Biomass_IVc_fish["Whiting (adult)", as.character(year)])
  ratio_IVcNS <- sum(sea_bass_area_IVc_year$CPUE_number_per_hour)/(sum(cod_area_IVc_year$CPUE_number_per_hour) + sum(whiting_area_IVc_year$CPUE_number_per_hour))
  bio_sharks_et_al["Sea bass", as.character(year)] <- ratio_IVcNS * (Biomass_IVc_fish["Cod (adult)", as.character(year)] + Biomass_IVc_fish["Whiting (adult)", as.character(year)])

  ratio_IVcNS <- sum(horse_mackerel_area_IVc_year$CPUE_number_per_hour)/sum(herring_area_IVc_year$CPUE_number_per_hour)
  bio_sharks_et_al["Horse mackerel", as.character(year)] <- ratio_IVcNS * Biomass_IVc_fish["Herring (adult)", as.character(year)]
  ratio_IVcNS <- sum(pelagic_area_IVc_year$CPUE_number_per_hour)/sum(herring_area_IVc_year$CPUE_number_per_hour)
  bio_sharks_et_al["Pelagic", as.character(year)] <- ratio_IVcNS * Biomass_IVc_fish["Herring (adult)", as.character(year)]

  ratio_IVcNS <- sum(sandeel_area_IVc_year$CPUE_number_per_hour)/sum(sandeel_area_1r_year$CPUE_number_per_hour)
  bio_sharks_et_al["Sandeels", as.character(year)] <- ratio_IVcNS * biomass_fish["Sandeel", as.character(year)]
}


Biomass_IVc_fish["Sharks",] <- bio_sharks_et_al["Sharks",]
Biomass_IVc_fish["Other gadoids",] <- bio_sharks_et_al["Other gadoids",]
Biomass_IVc_fish["Demersal fish",] <- bio_sharks_et_al["Demersals",]
Biomass_IVc_fish["Sea bass",] <- bio_sharks_et_al["Sea bass",]
Biomass_IVc_fish["Horse mackerel",] <- bio_sharks_et_al["Horse mackerel",]
Biomass_IVc_fish["Pelagic fish",] <- bio_sharks_et_al["Pelagic",]
Biomass_IVc_fish["Sandeels",] <- bio_sharks_et_al["Sandeels",]

# EMPIRICAL EQUATIONS
# cephalopods ------------------------------------------------------------------
# load csv files
cephalopods_CPUE_all_years <- read.csv("data/cephalopods CPUE per length per subarea.csv", header = TRUE, stringsAsFactors = FALSE)

#extract number of subarea to select correct area
cephalopods_CPUE_all_years$rowICES <- as.numeric(substr(cephalopods_CPUE_all_years$SubArea,1,2))

ratio_cephalopods <- array(NA, dim =c(1,21),list("cephalopods",c(1999:2019)))
for(year in unique(cephalopods_CPUE_all_years$Year)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 1999
  cephalopods_CPUE <- subset(cephalopods_CPUE_all_years,Year==year)
  NS_CPUE <- cephalopods_CPUE
  IVc_CPUE <- subset(cephalopods_CPUE, cephalopods_CPUE$rowICES > 30 & cephalopods_CPUE$rowICES < 36)

  # changing NA with 0 for calculation purpose
  IVc_CPUE[is.na(IVc_CPUE)] <- 0
  NS_CPUE[is.na(NS_CPUE)] <- 0

  # calculate ratio
  cephalopods_CPUE <- sum(IVc_CPUE$CPUE_number_per_hour)
  NS_CPUE2 <- sum(NS_CPUE$CPUE_number_per_hour)
  ratio_cephalopods[,as.character(year)] <- cephalopods_CPUE/NS_CPUE2
}
# calculation biomass tonnes/km2 for IVc
bio_cephalopods <- (mean(ratio_cephalopods[,-c(1:4)])*(0.08*570000))/63633.85614 # tonnes / km2 and 570000 area North Sea
### QUESTION: We estimated the biomass for squid and cuttlefish by calculating the mean ratio from 1999 until 2018
# between the abundance in the southern North Sea and the greater North Sea based on the ICES NS-IBTS data
# and multiplying this ratio with the total biomass of the North Sea model (Mackinson and Daskalov, 2007).

# Harbour porpoise -------------------------------------------------------
#density (ind/km2) from OSPAR - SCANSIII - Hammond et al. (2017) - Estimates of cetacean abundance in European Atlantic waters in summer 2016 from the SCANS-III aerial and shipboard surveys
#checked with ICES WGMME REPORT 2014 (in Belgian waters 0.26 - 2.11 ind/km)
# avg weight / ind from Trites et al (1999) NOT TRUE: BJORGE & TOLLEY (2009) see manuscript, confirmed with Steven
B_harbporp <- 0.607*55/1000
# Hammond et al 2002 - > 0.095 - 0.34 ind/km
0.095*55/1000
0.34*55/1000

# Crangon crangon ---------------------------------------
# estimate biomass crangon crangon
# Tulp et al (2016) estimate of Crangon crangon in max depth 30m, sNoSe (IVc) is plusminus max depth 30m everywhere (https://www.marineregions.org/maps.php?album=3747&pic=115811) => take t/km2 of study
x <- c(5432, 9263, 7716, 5324)
mean(x) # gemiddelde van 4 laatste beschikbare jaren = 6933.75
crangon_ad <- 6933.75/16186 #area from Tulp et al. (2016)

# benthos using BenBIO database ---------------------------------------------------
benthos_all <- read.csv("data/BenBIO database.csv", header = TRUE, stringsAsFactors = FALSE)
benthos_NS <- subset(benthos_all,benthos_all$Region == "North Sea"|benthos_all$Region == "English Channel")
benthos_Ska_Kat <- subset(benthos_all,benthos_all$Region == "Skagerrak"|benthos_all$Region == "Kattegat")

# conversion factor based on Ricciardi & Bourget (1998) - weight-to-weight conversion factorsfor marine benthic macroinvertebrates
bio_benthos <- array(NA, dim =c(3,2), list(c("macrobenthos", "meiobenthos", "megabenthos" ),c("biomass (g AFDM/m2)","biomass (g WW/m2)")))
for (i in unique(benthos_NS$Benthos)){
  biomass <- benthos_NS$Biomass[benthos_NS$Benthos==i]
  bio_benthos[rownames(bio_benthos) == i,1] <- mean(biomass)
  bio_benthos[rownames(bio_benthos) == i,2] <- (mean(biomass)/11.41)*100
}

bio_benthos2 <- array(NA, dim =c(3,1), list(c("macrobenthos", "meiobenthos", "megabenthos" ),"biomass (g WM/m2)"))
for (i in unique(benthos_Ska_Kat$Benthos)){
  biomass <- benthos_Ska_Kat$Biomass[benthos_Ska_Kat$Benthos==i]
  bio_benthos2[rownames(bio_benthos2) == i] <- mean(biomass)
}

total_bio_benthos <- sum(c(mean(c(bio_benthos[1,2],bio_benthos2[1,1])),bio_benthos[c(2,3),2]))

#######################################################################################################
# consumption of harbour porpoise based on Kastelein et al (1997) ------------------
QB_harb_porp <- (B_harbporp*area_IVc_km2*0.0675*365)/(B_harbporp*area_IVc_km2) # daily consumption of 6.75 % of body weight
# from excel file QB = 16.37647123, Stabler is 17.63

# consumption seabirds --------------------------------------------
#Log DR = -0.293+0.85 log W       Nilsson SG, Nilsson IN. 1976. Number, food and consumption, and fish predation by birds in Lake Mockeln, Southern Sweden. Ornis. Scand. 7:61-70.
QB_non_discard_seabirds <- (exp(-0.293+0.85*log(0.00162*area_IVc_km2*1000))/(0.00162*area_IVc_km2*1000))*365
#volgens excel file QB = 61.35721125
QB_discard_seabirds <- (exp(-0.293+0.85*log(0.0003555*area_IVc_km2*1000))/(0.0003555*area_IVc_km2*1000))*365
#volgens excel file QB = 77.03124617
#######################################################################################################

# collecting all requirements for empirical equations ---------------------------
discards_IVc <- read.csv("data/discard_tonnes_year_km2_2018.csv",header = TRUE,row.names = 1)
landing_tonnes_year_km <- read.csv("data/landing_tonnes_year_km2_2018.csv",header = TRUE,row.names = 1)
empirical_requirements <-  as.data.frame(array(NA, dim =c(37,17), list(c(rownames(discards_IVc)),c("Type","Biomass_area",'B',"L","D","C","F","Linf","Winf","Temp","K","Pf","Hd","M","PB","QB","PQ"))))
empirical_requirements[,"Type"] <- c(rep("toppredator",4),rep('fish',22),'invertebrate',rep('plankton',3),rep('invertebrate',4),"plankton","DOM","DOM")
emp_eq_info <- read.csv("data/empirical equations.csv",header = TRUE)
rownames(emp_eq_info) <- emp_eq_info$Species
#import data in empirical equation df
for (r in rownames(empirical_requirements)[which(rownames(empirical_requirements)%in%rownames(discards_IVc))]) {
  empirical_requirements[r,"D"] <- sum(discards_IVc[r,]*1000*area_IVc_km2) # discards in kg
}
for (r in rownames(empirical_requirements)[which(rownames(empirical_requirements)%in%rownames(landing_tonnes_year_km))]) {
  empirical_requirements[r,"L"] <- sum(landing_tonnes_year_km[r,]*1000*area_IVc_km2) #landings in kg
}
for (r in rownames(empirical_requirements)[which(rownames(empirical_requirements)%in%rownames(Biomass_IVc_fish))]) {
  empirical_requirements[r,"Biomass_area"] <- Biomass_IVc_fish[r,"2018"] * 1000 # biomass for area IVc in kg
}
for (r in rownames(empirical_requirements)[which(rownames(empirical_requirements)%in%rownames(Biomass_IVc_fish))]) {
  empirical_requirements[r,"B"] <- Biomass_IVc_fish[r,"2018"] / area_IVc_km2 # biomass in tonnes / km2 per year
}
for (r in rownames(empirical_requirements)[which(rownames(empirical_requirements)%in%rownames(emp_eq_info))]) {  # add info on K, Linf, Winf, ...
  for (c in colnames(empirical_requirements)[which(colnames(empirical_requirements)%in%colnames(emp_eq_info))]){
    empirical_requirements[r,c] <- emp_eq_info[r,c]
  }
}

# sum landings and discards per FG to obtain catch per FG
empirical_requirements$C <- empirical_requirements$L + empirical_requirements$D

#F is catches/BIO
empirical_requirements$F <- empirical_requirements$C/empirical_requirements$Biomass_area

# Fish empirical equations -----------------------------
# variables
#Linf in cm
#Winf in g
#Temp in ?C
#K in  y^-1, growth parameter from the von Bertalanffy Growth Function
#F in y^-1 (C/B)
#Pf is Pf = 1 for predators; PF = 0 for herbivores and detritivores
#Hd is carnivores = 0; non carnivore = 1

# functions
Production <- function(Linf, Winf, K, Temp, F) {
  if (!is.na(Linf)) {                                                           # converted from Pauly (1980), Temp in ?C
    M <- 10^(-0.0066 - 0.279*log10(Linf)+ 0.6543*log10(K) + 0.4634*log10(Temp))
  } else {
    M <- 10^(-0.2107 - 0.0824*log10(Winf)+ 0.6757*log10(K) + 0.4627*log10(Temp))
  }
  PB <- F + M
  return(c(M,PB))
}

Consumption <- function(Winf, Pf, Temp, Hd) {
  QB <- 10^(6.37 - 1.5045*(1000/(Temp+273.15)) -  0.168 * log10(Winf) + 0.1399*Pf + 0.2765* Hd)   # converted from Pauly (1990), Temp in °C, Hd herb 1 non-herb 0, Pf carn 1 non-carn 0
  return(QB)
}

# calculate M, PB, QB for fish
fish_subset <- empirical_requirements[which(!is.na(empirical_requirements$B)),]
for(r in rownames(fish_subset)){
  Linf = fish_subset[r,"Linf"]
  Winf = fish_subset[r,"Winf"]
  Temp = fish_subset[r,"Temp"]
  K = fish_subset[r,"K"]
  F =  fish_subset[r,"F"]
  Pf =  fish_subset[r,"Pf"]
  Hd = fish_subset[r,"Hd"]
  empirical_requirements[r,"M"] <- Production(Linf, Winf, K, Temp, F)[1]
  empirical_requirements[r,"PB"] <- Production(Linf, Winf, K, Temp, F)[2]
  empirical_requirements[r,"QB"] <- Consumption(Winf, Pf, Temp, Hd)
  empirical_requirements[r,"PQ"] <- empirical_requirements[r,"PB"]/empirical_requirements[r,"QB"]
}
empirical_requirements

#include harb porp, crangon, seabirds etc. B,PB,QB
empirical_requirements["Harbour porpoise","B"] <- B_harbporp
empirical_requirements["Harbour porpoise","PB"] <- 0.02   #Stäbler (2016)
empirical_requirements["Harbour porpoise","QB"] <- QB_harb_porp # or 17.63 from Stäbler (2016)
empirical_requirements["Seals","B"] <- 0.008 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Seals","PB"] <- 0.09 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Seals","QB"] <- 26.842 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Crangon (commercial size)","B"] <- crangon_ad
empirical_requirements["Crangon (commercial size)","PB"] <- 6.51 # Stäbler (2016)
empirical_requirements["Crangon (commercial size)","QB"] <- 10 # Stäbler (2016)
empirical_requirements["Seabirds (discard)","B"] <- 0.0003555 # literature (not year specific)
empirical_requirements["Seabirds (discard)","PB"] <- 0.102 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Seabirds (discard)","QB"] <- QB_discard_seabirds
empirical_requirements["Seabirds (non-discard)","B"] <- 0.00162 # literature (not year specific)
empirical_requirements["Seabirds (non-discard)","PB"] <- 1.119 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Seabirds (non-discard)","QB"] <- QB_non_discard_seabirds
empirical_requirements["Phytoplankton","B"] <- 7.5 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Phytoplankton","PB"] <- 286.7 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Pelagic fish","PB"] <- 4 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Pelagic fish","QB"] <- 10.19 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Pelagic fish","EE"] <- 0.98 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Squid & cuttlefish","B"] <- bio_cephalopods
empirical_requirements["Squid & cuttlefish","PB"] <- 11.025 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Squid & cuttlefish","QB"] <- 20 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Carnivorous zooplankton","PB"] <- 4 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Carnivorous zooplankton","EE"] <- 0.99 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Carnivorous zooplankton","PQ"] <- 0.32 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Herbivorous plankton (copepods)","B"] <- 16 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Herbivorous plankton (copepods)","PB"] <- 9.2 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Herbivorous plankton (copepods)","QB"] <- 30 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Gelatinous zooplankton","B"] <- 0.09120453 # Stäbler et al. (2016)
empirical_requirements["Gelatinous zooplankton","PB"] <- 2.858 # Stäbler et al. (2016)
empirical_requirements["Gelatinous zooplankton","PQ"] <- 0.45 # Stäbler et al. (2016)
empirical_requirements["Large crabs + shrimps","B"] <- 2.301928 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Large crabs + shrimps","PB"] <- 1.054394 # Stäbler et al. (2016)
empirical_requirements["Large crabs + shrimps","PQ"] <- 0.2000302 # Stäbler et al. (2016)
empirical_requirements["Benthos","B"] <- total_bio_benthos
empirical_requirements["Benthos","PB"] <- 1.06085 # Stäbler et al. (2016)
empirical_requirements["Benthos","PQ"] <- 0.2664477 # Stäbler et al. (2016)
empirical_requirements["Blue mussels (aquaculture)","B"] <-  0.055
empirical_requirements["Blue mussels (aquaculture)","PB"] <- 0.36 #Ecopath model of the northern Wadden Sea (Horn et al., 2020)
empirical_requirements["Blue mussels (aquaculture)","QB"] <- 2.67 #Ecopath model of the northern Wadden Sea (Horn et al., 2020)
empirical_requirements["Detritus","B"] <- 50 # North Sea mode Makinson & Dasdalov (2007)
empirical_requirements["Discards","B"] <- 0.0001 # North Sea mode Makinson & Dasdalov (2007)

write.csv(empirical_requirements,"data/Ecopath parameters_2018.csv")

