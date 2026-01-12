fd_TS = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/SBNS models/TS_SBNS_V24_TechnicalReport_Brel.csv"
# column names or to name the FG correctly
cols <- c("year",
          "Harbour porpoise",
          "Seals",
          "Seabirds (discard)",
          "Seabirds (non-discard)",
          "Sharks",
          "Rays",
          "Juvenile Cod",
          "Cod (adult)",
          "Juvenile Whiting",
          "Whiting (adult)",
          "Other gadoids",
          "Demersal fish",
          "Juvenile Herring",
          "Herring (adult)",
          "Sprat",
          "Mackerel",
          "Horse mackerel",
          "Sandeels",
          "Juvenile Plaice",
          "Plaice (adult)",
          "Dab",
          "Other flatfish",
          "Juvenile Sole",
          "Sole (adult)",
          "Sea Bass",
          "Pelagic fish",
          "Squid & cuttlefish",
          "Carnivorous zooplankton",
          "Herbivorous plankton (copepods)",
          "Gelatinous zooplankton",
          "Large crabs + shrimps",
          "Blue mussels (reefs)",
          "Blue mussels (aquaculture)",
          "Epifaunal macrobenthos (mobile grazers)",
          "Infaunal macrobenthos",
          "Crangon",
          "Small mobile epifauna (swarming crustaceans)",
          "Small infauna (polychaetes)",
          "Sessile epifauna",
          "Meiofauna",
          "Phytoplankton",
          "Detritus",
          "Discards")

# read csv file and clean up unwanted info from TS csv, only validation data wanted, no drivers
param_TS_df <- read.csv(file = fd_TS, header = TRUE, sep = ',')
param_TS_df_fleets <- param_TS_df[,c(1,which(param_TS_df[3,]==3))]
param_TS_df <- param_TS_df[,-which(param_TS_df[3,]==3)]
# give correct FG name to the FG in TS
for(i in 2: ncol(param_TS_df)){
  param_TS_df[2,i] <- cols[param_TS_df[2,i]+1]
}

param_TS_df_fleets[2,] <- c("poolcode", "Comm. Beam", "Comm. Demersal", "Comm. Dredge", "Comm. Hooks", "Comm. Nets", "Comm. Pelagic",
  "Comm. Pots", "Comm. Shrimp")


# reformatting df
timeseries <- data.frame("year" = as.Date(NA), "fg" = NA, "type" = NA, "weight" = NA, "value" = NA)
timeseries_fleets <- data.frame("year" = as.Date(NA), "fg" = NA, "type" = NA, "weight" = NA, "value" = NA)

for (i in 2:ncol(param_TS_df)){
  df <- data.frame("year" = seq.Date(from = as.Date("1991-01-01"), by = "year", length.out = 33), "fg" = param_TS_df[2,i], "type" = param_TS_df[3,i],
                   "weight" =  param_TS_df[1,i], "value" = param_TS_df[4:nrow(param_TS_df),i])

  timeseries <- rbind(timeseries, df)
  }

for (i in 2:ncol(param_TS_df_fleets)){
df2 <- data.frame("year" = seq.Date(from = as.Date("1991-01-01"), by = "year", length.out = 33), "fg" = param_TS_df_fleets[2,i], "type" = param_TS_df_fleets[3,i],
                  "weight" =  param_TS_df_fleets[1,i], "value" = param_TS_df_fleets[4:nrow(param_TS_df_fleets),i])
timeseries_fleets <- rbind(timeseries_fleets, df2)
}

timeseries <- timeseries[-1,]
timeseries_fleets <- timeseries_fleets[-1,]

timeseries$type[which(timeseries$type== 6)] <- 'absolute catch'
timeseries$type[which(timeseries$type== 4)] <- 'fishing mortality'
timeseries_fleets$type[which(timeseries_fleets$type== 3)] <- 'fishing effort'
timeseries$type[which(timeseries$type== 0)] <- 'relative biomass'
timeseries$type[which(timeseries$type== 1)] <- 'absolute biomass'

timeseries$value <- as.numeric(timeseries$value)
timeseries$fg <- factor(timeseries$fg, levels = unique(timeseries$fg))
timeseries_fleets$value <- as.numeric(timeseries_fleets$value)
timeseries_fleets$fg <- factor(timeseries_fleets$fg, levels = unique(timeseries_fleets$fg))

# plots
subs <- subset(timeseries, type == 'absolute biomass')
annotations <- subset(subs, year == '2018-01-01')
max_df <- na.omit(subs) %>%
  group_by(fg) %>%
  summarise(value = max(value)+0.00)
annotations <- merge(annotations, max_df, "fg")
annotations <- annotations %>%
  arrange(fg)

abs_B <- ggplot() +
  geom_line(data = subs, aes(x = year, y = value,linetype = "solid"), colour = "black", linewidth = 0.75 ) +
  geom_point(data = subs, aes(x = as.Date(year), y = value, shape = "absolute biomass"), colour = "black", size = 2) +
  geom_text(data = annotations, aes(x = year, y = value.y), label = paste0("weight: ",annotations$weight), color = "black", size = 3) +
  facet_wrap(vars(fg), ncol = 3, scales = "free_y")+
  scale_linetype_manual(values = c("solid"),   # Custom linetypes
                        labels = c("absolute biomass"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("absolute biomass" = 16),
                     labels = "absolute biomass",
                     name = NULL)+
  theme_bw() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(),
        legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.text.x = element_text(colour="black",size=11.5),
        axis.text.y = element_text(colour="black",size=14),
        axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "", y= expression(bold(paste("Biomass(t  ",km^-2,")"))))

ggsave("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Technical report/absolute_biomass.png",
       abs_B, width = 5, height = 3.5, dpi = 800)


subs <- subset(timeseries, type == 'relative biomass')
subs <- subset(subs, fg %in% levels(subs$fg)[10:20])
annotations <- subset(subs, year == '2018-01-01')
max_df <- na.omit(subs) %>%
  group_by(fg) %>%
  summarise(value = max(value)+0.00)
annotations <- merge(annotations, max_df, "fg")
annotations <- annotations %>%
  arrange(fg)

rel_B <- ggplot() +
  geom_line(data = subs, aes(x = year, y = value,linetype = "solid"), colour = "black", linewidth = 0.75 ) +
  geom_point(data = subs, aes(x = year, y = value, shape = "relative biomass"), colour = "black", size = 2) +
  geom_text(data = annotations, aes(x = year, y = value.y), label = paste0("weight: ",annotations$weight), colour = "black", size = 4) +
  facet_wrap(vars(fg), ncol = 3,nrow = 3, scales = "free_y")+
  scale_linetype_manual(values = c("solid"),   # Custom linetypes
                        labels = c("relative biomass"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("relative biomass" = 16),
                     labels = "relative biomass" ,
                     name = NULL)+
  theme_bw() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(),
        legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.text.x = element_text(colour="black",size=11.5),
        axis.text.y = element_text(colour="black",size=14),
        axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "", y= "Relative Biomass")

ggsave("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Technical report/relative_biomass.png",
       rel_B, width = 20, height = 14, dpi = 800)

subs <- subset(timeseries, type == 'absolute catch')
subs <- subset(subs, fg %in% levels(subs$fg)[13:20])
annotations <- subset(subs, year == '2018-01-01')
max_df <- na.omit(subs) %>%
  group_by(fg) %>%
  summarise(value = max(value)+0.00)
annotations <- merge(annotations, max_df, "fg")
annotations <- annotations %>%
  arrange(fg)

abs_C <- ggplot() +
  geom_line(data = subs, aes(x = year, y = value,linetype = "solid"), colour = "black", linewidth = 0.75 ) +
  geom_point(data = subs, aes(x = as.Date(year), y = value, shape = "absolute catch"), colour = "black", size = 2) +
  geom_text(data = annotations, aes(x = as.Date("2008-01-01"), y = value.y), label = paste0("weight: ",annotations$weight), colour = "black", size = 4) +
  facet_wrap(vars(fg), ncol = 3,nrow = 3, scales = "free_y")+
  scale_linetype_manual(values = c("solid"),   # Custom linetypes
                        labels = c("absolute catch"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("absolute catch" = 16),
                     labels = "absolute catch" ,
                     name = NULL)+
  theme_bw() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(),
        legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.text.x = element_text(colour="black",size=11.5),
        axis.text.y = element_text(colour="black",size=14),
        axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "", y= expression(bold(paste("Catch (t  ",km^-2, y^-1,")"))))

ggsave("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Technical report/catch.png",
       abs_C, width = 20, height = 15, dpi = 800)


subs <- subset(timeseries, type == 'fishing mortality')
subs <- subset(subs, fg != 'Rays')
annotations <- subset(subs, year == '2018-01-01')
max_df <- na.omit(subs) %>%
  group_by(fg) %>%
  summarise(value = max(value)+0.00)
annotations <- merge(annotations, max_df, "fg")
annotations <- annotations %>%
  arrange(fg)

fish_M <- ggplot() +
  geom_line(data = subs, aes(x = year, y = value,linetype = "solid"), colour = "black", linewidth = 0.75 ) +
  geom_point(data = subs, aes(x = as.Date(year), y = value, shape = "fishing mortality"), colour = "black", size = 2) +
  #geom_text(data = annotations, aes(x = year, y = value.y), label = paste0("weight: ",annotations$weight), colour = "black", size = 3) +
  facet_wrap(vars(fg), ncol = 4, scales = "free_y")+
  scale_linetype_manual(values = c("solid"),   # Custom linetypes
                        labels = c("fishing mortality"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("fishing mortality" = 16),
                     labels = "fishing mortality" ,
                     name = NULL)+
  theme_bw() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(),
        legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.text.x = element_text(colour="black",size=11.5),
        axis.text.y = element_text(colour="black",size=14),
        axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "", y= "Fishing Mortality")

ggsave("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Technical report/fishing_mortality.png",
       fish_M, width = 10, height = 7, dpi = 800)

subs <- timeseries_fleets
annotations <- subset(subs, year == '2018-01-01')
max_df <- na.omit(subs) %>%
  group_by(fg) %>%
  summarise(value = max(value)+0.00)
annotations <- merge(annotations, max_df, "fg")
annotations <- annotations %>%
  arrange(fg)

fish_E <- ggplot() +
  geom_line(data = subs, aes(x = year, y = value,linetype = "solid"), colour = "#354d9b", linewidth = 2 ) +
  #geom_point(data = subs, aes(x = as.Date(year), y = value, shape = "fishing effort"), colour = "#354d9b", size = 2) +
  #geom_text(data = annotations, aes(x = year, y = value.y), label = paste0("weight: ",annotations$weight), colour = "black", size = 3) +
  facet_wrap(vars(fg), ncol = 4, scales = "free_y")+
  scale_linetype_manual(values = c("solid"),   # Custom linetypes
                        labels = c("fishing effort"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("fishing effort" = 16),
                     labels = "fishing effort" ,
                     name = NULL)+
  theme_bw() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(),
        legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.text.x = element_text(colour="black",size=14),
        axis.text.y = element_text(colour="black",size=14),
        axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "", y= "Fishing effort")

ggsave("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Ecosim paper/fishing_mortality.png",
       fish_M, width = 10, height = 7, dpi = 800)
