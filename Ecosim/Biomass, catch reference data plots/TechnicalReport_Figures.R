library(ggplot2)
library(readxl)
library("dplyr")
library("tidyr")
library(patchwork)

catch <- read_excel("CatchTimeSeriesUpdates.xlsx")

p1 <- ggplot(catch, aes(x = year)) +
  geom_line(aes(y = Dab_u), color = "brown1", linewidth = 1.5) +
  geom_line(aes(y = Dab), color = "steelblue3", linewidth = 1) + 
  xlab(" ") + ylab("Relative catch") + labs(title = "Dab") +
  theme_bw() + theme(text = element_text(size = 14)) 

p2 <- ggplot(catch, aes(x = year)) +
  geom_line(aes(y = OtherFlatfish_u), color = "brown1", linewidth = 1.5) +
  geom_line(aes(y = OtherFlatfish), color = "steelblue3", linewidth = 1) + 
  theme_bw() + xlab(" ") + ylab(" ") + labs(title = "Other flatfish")  + theme(text = element_text(size = 14)) 

p3 <- ggplot(catch, aes(x = year)) +
  geom_line(aes(y = OtherGadoids_u), color = "brown1", linewidth = 1.5) +
  geom_line(aes(y = OtherGadoids), color = "steelblue3", linewidth = 1) + 
  theme_bw() + xlab(" ") + ylab (" ") + labs(title = "Other gadoids") + theme(text = element_text(size = 14)) 

p4 <- p1 + p2 + p3
p4

melted_catch <- tidyr::pivot_longer(catch, cols = -year, names_to = "Species", values_to = "Catch")
melted_catch %>%
  mutate(status = case_when(
    Species == "Other gadoids_u" ~ "new",
    Species == "Dab_u" ~ "new",
    Species == "Other flatfish_u" ~ "new",
    Species == "Other gadoids" ~ "old",
    Species == "Dab" ~ "old",
    Species == "Other flatfish" ~ "old"
  )) -> melted_catch

melted_catch$Species <- gsub("_u", "", melted_catch$Species)

pcatchupdate <- ggplot(melted_catch, aes(x = year, y = Catch, color = status)) +
  geom_line(aes(size = status)) +
  facet_wrap(~Species, nrow = 1, scales = "free_y") +
  labs(x = "Year", y = "Relative Catch") +
  theme_bw() + theme(text = element_text(size = 14),
                     legend.position = "none") +
  theme(strip.text.x = element_text(size = 12)) +
  scale_size_manual(values = c("old" = 1, "new" = 1.5)) +  
  scale_color_manual(values = c("old" = "steelblue3", "new" = "brown1"))

pcatchupdate

ggsave("CatchTimeSeriesUpdates.png", pcatchupdate, width = 12, height = 4, dpi = 500)

############# Catch
catch_all <- read_excel("CatchTimeSeriesAll.xlsx")
catch_all$Sandeels <- as.numeric(catch_all$Sandeels)

melted_data <- tidyr::pivot_longer(catch_all, cols = -year, names_to = "Species", values_to = "Catch")

p5 <- ggplot(melted_data, aes(x = year, y = Catch)) +
  geom_line() + geom_point() +
  facet_wrap(~Species, nrow = 5, scales = "free_y") +
  labs(x = "Year", y = "Relative Catch") +
  theme_bw() + theme(text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 10)) #+ # Adjust text size, angle, and alignment for species names
  
ggsave("CatchTimeSeriesAll.png", p5, width = 10, height = 11, dpi = 1000)

############ BIOMASS

B_all <- read_excel("BiomassTimeSeriesAll.xlsx")

melted_data_B <- tidyr::pivot_longer(B_all, cols = -year, names_to = "Species", values_to = "Biomass")


p6 <- ggplot(melted_data_B, aes(x = year, y = Biomass)) +
  geom_line() + geom_point() +
  facet_wrap(~Species, nrow = 4, scales = "free_y") +
  labs(x = "Year", y = "Relative Biomass") +
  theme_bw() + theme(text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 10)) #+ # Adjust text size, angle, and alignment for species names

ggsave("BiomassTimeSeriesAll.png", p6, width = 10, height = 11, dpi = 1000)


############ FORCING FUNCTIONS

FF <- read.csv("ForcingFuntions.csv")
FF <- FF[1:5]
colnames(FF) <- c("pH", "Net primary production [mgC-3.m-2.d-1]", "Sea water temperature [°C]", "Salinity [ppm]")
FF_long <- tidyr::pivot_longer(FF, cols = -year, names_to = "Parameter", values_to = "Value")


p7 <- ggplot(FF_long, aes(x = year, y = Value)) +
  geom_line() +
  facet_wrap(~Parameter, nrow = 4, scales = "free_y") +
  labs(x = "Year") +
  theme_bw() + theme(text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 14))

ggsave("FFTimeSeries.png", p7, width = 6, height = 9, dpi = 1000)











