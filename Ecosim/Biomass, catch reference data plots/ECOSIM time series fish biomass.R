# Calculations for updated time-series for fish stock biomass to use in EcoSim
# 1991-2022 based on ICES report: https://standardgraphs.ices.dk/stockList.aspx

# Stock assesments used (midpoint spawning stock biomass):
# Herring: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17752
# sandeel: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17718
# sprat: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17752
# cod: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17652
# plaice: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17820
# sole: https://standardgraphs.ices.dk/ViewCharts.aspx?key=18458
# sea bass: https://standardgraphs.ices.dk/ViewCharts.aspx?key=18142
# whiting: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17795
# mackerel: https://standardgraphs.ices.dk/ViewCharts.aspx?key=17509

# note on CPUE data: DATRAS does not allow you to download a lot in one go, hence all the different files...

# Preparation Calculation -----------------------------------------------------------------
area_IVc_km2 <- 63633.85614 # area SBNS in km2

# CPUE
# by age to calculate ratio greater NS-IVc and estimate Biomass in next step ----------------------------------------------------
# load CPUE csv file (1991-2022)
# source: https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
NS_CPUE_all_years <- read.csv("data/update_2022/CPUE per age per subarea_COMBINED_old_and_new.csv", header = TRUE, stringsAsFactors = FALSE)
# note: the new data missed CPUE for the Englisch channel before 2007, so i added 2020-2022 to Stevens original CPUE
  # define the areas
  NS_CPUE_all_years$rowICES <- as.numeric(substr(NS_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area
  NS_CPUE_all_years <- subset(NS_CPUE_all_years, Species != 'Pollachius virens') # not of interest
  NS_CPUE_all_years <- subset(NS_CPUE_all_years, Species != 'Trisopterus esmarkii') # not of interest
  NS_CPUE_all_years <- subset(NS_CPUE_all_years, Species != 'Melanogrammus aeglefinus') # not of interest

  area_EC_7d <- subset(NS_CPUE_all_years,rowICES < 31)
  area_IVc <- subset(NS_CPUE_all_years,rowICES > 30 & rowICES < 36)
  area_NS <- subset(NS_CPUE_all_years,rowICES > 30 & rowICES < 52 & SubArea!="47G0" & SubArea!="43F9" & SubArea!="44F8" & SubArea!="44F9" & SubArea!="44G1" & SubArea!="45F8" & SubArea!="45F9" & SubArea!="45G1" & SubArea!="46F9" & SubArea!="46G1")
  area_Ska_3a_20 <- subset(NS_CPUE_all_years, SubArea=="44F7"|SubArea=="43F8"|SubArea=="47G0"|SubArea=="43F9"|SubArea=="44F8"|SubArea=="44F9"|SubArea=="44G1"|SubArea=="45F8"|SubArea=="45F9"|SubArea=="45G1"
                         |SubArea=="46F9"|SubArea=="46G1")

# information on plaice to use in calculations from WGNSSK ices reports: https://ices-library.figshare.com/articles/report/Working_Group_on_the_Assessment_of_Demersal_Stocks_in_the_North_Sea_and_Skagerrak_WGNSSK_/22643143?file=43722618
plaice_stock_numbers <- read.csv("data/update_2022/plaice_stock_numbers_by_age_2022.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1)
plaice_stock_numbers <- plaice_stock_numbers*1000 # as number in thousand
plaice_stock_weight <- read.csv("data/update_2022/plaice_stock_weight_by_age_2022.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1) #weigth in kg
colnames(plaice_stock_numbers) <- c(1:10)
colnames(plaice_stock_weight) <- c(1:10)

# load historical landings (will be used for ratio of pelagic fish)
# source: https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx
historical_landings <- read.csv("data/ICES_1950-2010.csv", header = TRUE, stringsAsFactors = FALSE)

#create file to store ratio per species
### once for cod and once for plaice?
ratio_of_biomass <- array(NA, dim =c(6,32), list(c("Gadus morhua", "Clupea harengus","Merlangius merlangus","Scomber scombrus","Sprattus sprattus","Pleuronectes platessa"),c(1991:2022))) # use scientific names as in CPUE file
plaice_TSB_area_IVc <- array(NA, dim =c(1,32),list("plaice TSB",c(1991:2022)))

for (year in c(1991:2022)){
  #Subsetting in R instead of doing it manually in excel
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


#1:
# I think this can be skipped for ecosim?
# ratio_of_biomass['Whiting (adult)',"1991"] <- mean(ratio_of_biomass['Whiting (adult)',c(1:5)]) # use  mean of first 5 years to have better estimate

#2:
#cod Historical landingsratio -> better estimate
# this may be the case for 1991, but is it the case for all years?
# also: of course there are no historical landings for recent years, adjusting this section will be annoying...
# HENCE: FOR NOW I SKIP THIS SECTION

# landing_cod_IV <- subset(historical_landings, Species == 'Atlantic cod')
# landing_cod_IV <- subset(landing_cod_IV, Division == "IV a"|Division == "IV b"|Division == "IV c"|Division == "VII d"|Division == "III a")
# landing_cod_IVc <- subset(landing_cod_IV,Division == "IV c")
# sum_landing_cod_IV <- rep(NA,64)
# sum_landing_cod_IVc <- rep(NA,64)
# for (i in c(40:64)){      # create warnings message about introducing NAs but it's okay as they are removed
  # sum_landing_cod_IV[i] <- sum(na.omit(as.numeric(landing_cod_IV[,i])))
  # sum_landing_cod_IVc[i] <- sum(na.omit(as.numeric(landing_cod_IVc[,i])))
# }
# sum_landing_cod_IV <- na.omit(sum_landing_cod_IV)
# sum_landing_cod_IVc <- na.omit(sum_landing_cod_IVc)
# ratio_landingIVc_IV <- sum_landing_cod_IVc/sum_landing_cod_IV
# ratio_of_biomass["Cod (adult)","1991"] <- mean(ratio_landingIVc_IV[1:10]) # 10-year mean from 1986-1995


# calculate biomass for 1991-2022
# here you apply the ratio between the greater North Sea and area IVc, because stock assesments are for the entire North Sea
biomass_fish <- read.csv("data/update_2022/Biomass_fish_1991_2022.csv", header = TRUE, stringsAsFactors = FALSE,row.names=1) #load biomass csv file
colnames(biomass_fish) <- c("Herring (adult)","Sandeel","Sprat","Cod (adult)","Plaice (adult)","Sole (adult)","Sea Bass","Whiting (adult)","Mackerel") # rename biomass columns
biomass_fish <- as.data.frame(t(biomass_fish))

# correct NEA Mackerel biomass for North Sea stock
### THIS DOES NOT SEEM CORRECT
### ASK STEVEN
# mackerel_NS_portion <- read.csv("data/Mackerel NS portion (WKPELA 2014).csv", header = TRUE, stringsAsFactors = FALSE) #load biomass csv file
# colnames(mackerel_NS_portion) <- c(1991:2022)
# biomass_fish["Mackerel",] <- (biomass_fish["Mackerel",] * mackerel_NS_portion[,as.character(c(1991:2022))]) / 2 # divide by 2 because only 6 months in NS during winter/autumn (Jansen (2014))
# ALTENATIVE METHOD is already incorporated in previous section

# correct whiting biomass
biomass_fish["Whiting (adult)",c(1:5)] <- (biomass_fish["Whiting (adult)",c(1:5)]-(biomass_fish["Whiting (adult)",c(1:5)]*0.08109553)) # correction on biomass for IBTS not trawling in VIId

# apply ratio
Biomass_IVc_fish <- as.data.frame(array(NA,c(nrow(ratio_of_biomass),ncol(ratio_of_biomass)), list(c(rownames(ratio_of_biomass)),c(colnames(ratio_of_biomass)))))
for (species in rownames(ratio_of_biomass)){
  Biomass_IVc_fish[species,] <- ratio_of_biomass[species,]*biomass_fish[species,]
}


# Sole ------------------------------------------------------------------
# CPUE ratio NS-IVc: why not do this in the previous section?
# also: it appears we only actually use sole? Plaice is already included in the previous section)
# load csv files
Sole_plaice_CPUE_all_years <- read.csv("data/update_2022/plaice and sole CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
Sole_CPUE_all_years <- subset(Sole_plaice_CPUE_all_years,Sole_plaice_CPUE_all_years$Species == "Solea solea")

# define the areas
Sole_CPUE_all_years$rowICES <- as.numeric(substr(Sole_CPUE_all_years$SubArea,1,2)) # extract number of subarea to select correct area

# only interested in sole (adult) for ssb
sole_area_IVc <- subset(Sole_CPUE_all_years,rowICES > 30 & rowICES < 36 & LngtClass > 209) # Length maturity (Fishbase) is 21.3 cm, so equal or bigger then 210 mm is sole (adult)
sole_area_NS <- subset(Sole_CPUE_all_years,rowICES > 30 & rowICES < 52 & SubArea!="47G0"& SubArea!="43F9" & SubArea!="44F8"
                       & SubArea!="44F9" & SubArea!="44G1" & SubArea!="45F8" & SubArea!="45F9" & SubArea!="45G1"
                       & SubArea!="46F9" & SubArea!="46G1" & LngtClass > 209) # Length maturity (Fishbase) is 21.3, so equal or bigger then 210 mm is sole (adult)

bio_sole <- array(NA, dim =c(1,32), list("Sole (adult)",c(1991:2022)))
for (year in c(1991:2022)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 1991
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
other_flatfish_CPUE_all_years <- read.csv("data/update_2022/other_flatfish CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
dab_CPUE_all_years <- read.csv("data/update_2022/dab CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
rays_CPUE_all_years <- read.csv("data/update_2022/rays CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
Sole_plaice_CPUE_all_years <- read.csv("data/update_2022/plaice and sole CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
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


bio_other_flatfish <- array(NA, dim =c(1,32), list("other_flatfish",c(1991:2022)))
bio_dab <- array(NA, dim =c(1,32), list("dab",c(1991:2022)))
bio_rays <- array(NA, dim =c(1,32), list("rays",c(1991:2022)))
for (year in c(1991:2022)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 1991
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

# sharks, other gadoids, demersal, sea bass, horse mackerel & pelagic  ------------------------------------------------------------------
# load csv files
shark_et_al_CPUE_all_years <- read.csv("data/update_2022/shark et al CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)
demersal_CPUE_all_years <- read.csv("data/update_2022/demersal CPUE per length per subarea_2024-03-07.csv", header = TRUE, stringsAsFactors = FALSE)

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


bio_sharks_et_al <- array(NA, dim =c(7,32), list(c("Sharks","Other gadoids","Pelagic","Sea bass", "Horse mackerel","Sandeels","Demersals"),c(1991:2022)))

for (year in c(1991:2022)){
  #Subsetting in R instead of doing it manually in excel
  #year <- 1991
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

write.csv(Biomass_IVc_fish, "data/update_2022/stock biomass estimate fish 1991-2022.csv")
