library(tidyverse)
library(modelsummary)
library(dplyr)
library(estimatr)
library(patchwork)

#import data
setwd("/Users/peterq/korea\ HPI\ data/data")
hpi_type <- read_csv("hpi_type_copy.csv")
hpi_scale <- read_csv("hpi_scale_copy.csv")

hpi_type <- hpi_type %>% select(-UNIT, -Item)
hpi_scale <- hpi_scale %>% select(-UNIT, -Item)

#===============================================================================

#Creating Subsets From Imported Data Function

create_subset <- function(input_df, remove_cols, filter_region_value) {
  
  df_filtered <- subset(input_df, Region %in% filter_region_value) # creates df with only certain regions

  df_long <- df_filtered %>% 
    pivot_longer(cols = -remove_cols,
                 names_to = "date",
                 values_to = "SalesPriceIndex") # makes df into long format

  df_clean <- df_long %>% 
    mutate(date = gsub(" Month", "", date)) #removes ' Month' from date column string values
  
  df_clean$DateObjs <- as.Date(paste0(df_clean$date, ".01"), format = "%Y.%m.%d") #creates Date objects
  
  df_clean$Year <- year(df_clean$DateObjs) #creates Year column
  df_clean$Month <- month(df_clean$DateObjs) #creates Month column
  df_clean <- df_clean %>% select(-date) #removes date column

  df_final <- df_clean %>% drop_na(SalesPriceIndex)
  
  return(df_final)
}

#===============================================================================

# Whole Country Subset (from hpi_type data)
remove_cols <- c("Type", "Region")
filter_region_value <- c("TheWholeCountry")

type_wholecountry <- create_subset(input_df = hpi_type, remove_cols, filter_region_value) 

# Whole Country Subset (from hpi_scale data)
remove_cols <- c("Type", "Region", "Scale")
filter_region_value <- c("TheWholeCountry")

scale_wholecountry <- create_subset(input_df = hpi_scale, remove_cols, filter_region_value) 

# Provinces Subset (from hpi_type data)
remove_cols <- c("Type", "Region")
filter_region_value <- c('Gyeonggi', 'Gangwon', 'Chungbuk', 'Chungnam', 'Gyeongbuk',
                         'Gyeongnam', 'Jeju', 'Jeonbuk', 'Jeonnam')

type_provinces <- create_subset(input_df = hpi_type, remove_cols, filter_region_value)

# Provinces Subset (from hpi_scale data)
remove_cols <- c("Type", "Region", "Scale")
filter_region_value <- c('Gyeonggi', 'Gangwon', 'Chungbuk', 'Chungnam', 'Gyeongbuk',
                         'Gyeongnam', 'Jeju', 'Jeonbuk', 'Jeonnam')

scale_provinces <- create_subset(input_df = hpi_scale, remove_cols, filter_region_value) 

# Seoul Subset (from type data)
remove_cols <- c("Type", "Region")
filter_region_value <- c('Non-SeoulMetropolitanArea', 'NorthernSeoul', 'Seoul', 'SeoulMetropolitanArea', 'SouthernSeoul')

type_seoul <- create_subset(input_df = hpi_type, remove_cols, filter_region_value)

# Seoul Subset (from scale data)
remove_cols <- c("Type", "Region", "Scale")
filter_region_value <- c('Non-SeoulMetropolitanArea', 'NorthernSeoul', 'Seoul', 'SeoulMetropolitanArea', 'SouthernSeoul')

scale_seoul <- create_subset(input_df = hpi_scale, remove_cols, filter_region_value) 

#===============================================================================

# Plots (using Whole Country Type subset)

#plot, combining all Types of housing

max_spi <- max(type_wholecountry$SalesPriceIndex, na.rm = TRUE)

ggplot(data = type_wholecountry, mapping = aes(x = DateObjs, y = SalesPriceIndex, color = Type)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Types of Housing in The Whole Country (2012-01-01 to 2025-02-01)") +
  theme_minimal() +
  scale_x_date(breaks = as.Date(paste0(2012:2025, "-01-01")), date_labels = "%Y") +
  scale_y_continuous(breaks = c(seq(0, max_spi-1, by = 5), max_spi), 
                     position = "right", 
                     labels = function(x) ifelse(x == max_spi, paste0("Max: ", x), x)) +
  geom_hline(yintercept=max_spi, linetype="dashed")

#===============================================================================

# Plots (using Province Type subset)

# 9 Province

#plot, subset Total for 9 Provinces
ggplot(data = subset(type_provinces, Type == "Total"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Region, color = Region)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.75,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for All Types of Housing in each Province (2012-01-01 to 2025-02-01)")

#plot, subset Apartments for 9 Provinces
ggplot(data = subset(type_provinces, Type == "Apartments"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Region, color = Region)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.75,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Apartments in each Province (2012-01-01 to 2025-02-01)")

#plot, subset Row Houses for 9 Provinces
ggplot(data = subset(type_provinces, Type == "Row Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Region, color = Region)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.9,0.75)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Row Houses in each Province (2012-01-01 to 2025-02-01)")

#plot, subset Detached Houses for 9 Provinces
ggplot(data = subset(type_provinces, Type == "Detached Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Region, color = Region)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.75,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Detached Houses in each Province (2012-01-01 to 2025-02-01)")

#===============================================================================

# Plots (using Seoul Type subset)

#plot, subset Total Seoul 
ggplot(data = subset(type_seoul, Type == "Total" & Region == "Seoul"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex)) +
  geom_line(size = 1, color = "seagreen1") +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for All Types of Housing in Seoul (2012-01-01 to 2025-02-01)")

#===============================================================================
# geom_vline(xintercept = as.Date(c("2021-06-01","2017-11-01")), linetype = "dashed", color = "black") +
# 
# # Get the list of all built-in color names in R
# all_r_colors <- colors()
# 
# # Generate random samples of specified lengths
# colors_6_r <- sample(all_r_colors, 6)
# colors_4_r <- sample(all_r_colors, 4)
# colors_3_r <- sample(all_r_colors, 3)
# 
# wc_apts <-
# wc_rowh <-
# wc_dh <-
# wc_apts/wc_rowh/wc_dh
# 
# credit <- function() {
#   return(makeFootnote("\n\nData: KOSIS. https://kosis.kr/index/index.do"))
# }

# Plots (using whole country subset)

#plot, subset apartments by scale 
colors = c("salmon", "gold", "aquamarine", "darkblue", "violet", "lightpink")
ggplot(data = subset(scale_wc, Type == "Apartments"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Scale, color = Scale)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Apartments by Scale for The Whole Country (2012-01-01 to 2025-02-01)")

#plot, subset row houses by scale
colors = c("firebrick", "lightgreen", "dodgerblue", "deeppink2")
ggplot(data = subset(scale_wc, Type == "Row Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Scale, color = Scale)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.2)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Row Houses by Scale for The Whole Country (2012-01-01 to 2025-02-01)")

#plot, subset detached houses by scale
colors = c("cyan2", "goldenrod1", "maroon3")
ggplot(data = subset(scale_wc, Type == "Detached Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Scale, color = Scale)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Detached Houses by Scale for The Whole Country (2012-01-01 to 2025-02-01)")

#===============================================================================

# Plots (using province combination subset)

# Gyeonggi Province

#plot, subset apartments in Gyeonggi by scale 
ggplot(data = subset(scale_provinces, Region == "Gyeonggi" & Type == "Apartments"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Type, color = Scale)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.75,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Apartments in Gyeonggi Province by Scale (2012-01-01 to 2025-02-01)")

#plot, subset row houses in Gyeonggi by scale
colors = c("firebrick", "lightgreen", "dodgerblue", "deeppink2")
ggplot(data = subset(scale_provinces, Region == "Gyeonggi" & Type == "Row Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Scale, color = Scale)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.2)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Row Houses in Gyeonggi Province by Scale (2012-01-01 to 2025-02-01)")

#plot, subset detached houses by scale
colors = c("cyan2", "goldenrod1", "maroon3")
ggplot(data = subset(scale_provinces, Region == "Gyeonggi" & Type == "Detached Houses"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Scale, color = Scale)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Detached Houses in Gyeonggi Province by Scale (2012-01-01 to 2025-02-01)")

# Gangwon Province

#plot, subset apartments in Gangwon by scale 
ggplot(data = subset(scale_provinces, Region == "Gangwon" & Type == "Apartments"), 
       mapping = aes(x = DateObjs, y = SalesPriceIndex, fill = Type, color = Scale)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.81,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Apartments in Gangwon Province by Scale (2012-01-01 to 2025-02-01)")

#===============================================================================

