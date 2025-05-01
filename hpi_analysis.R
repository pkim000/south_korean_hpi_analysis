library(tidyverse)
library(modelsummary)
library(dplyr)
library(estimatr)

#import data
setwd("/Users/peterq/korea\ HPI\ data/data")
hpi_type <- read_csv("hpi_type_copy.csv")
hpi_scale <- read_csv("hpi_scale_copy.csv")

#===============================================================================

# Whole Country Df (from hpi_type data)

#cleaning data, subsetting, formatting
hpi_type <- hpi_type %>% select(-UNIT, -Item)
type_wc <- subset(hpi_type, Region=="TheWholeCountry") #df for hpi scale df where region = WholeCountry

#changing date columns to rows with corresponding spi values
type_wc <- type_wc %>% 
  pivot_longer(cols = -c(Type, Region),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
type_wc <- type_wc %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
type_wc$DateObjs <- as.Date(paste0(type_wc$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
type_wc$Year <- year(type_wc$DateObjs)
type_wc$Month <- month(type_wc$DateObjs) 
type_wc <- type_wc %>% select(-date) #drops date column

#drops NA values 
type_wc <- type_wc %>% drop_na(SalesPriceIndex)

#===============================================================================

# Whole Country Df (from hpi_scale data)

#cleaning data, subsetting, formatting
hpi_scale <- hpi_scale %>% select(-UNIT, -Item)
scale_wc <- subset(hpi_scale, Region=="TheWholeCountry") #df for hpi scale df where region = WholeCountry

#changing date columns to rows with corresponding spi values
scale_wc <- scale_wc %>% 
  pivot_longer(cols = -c(Type, Region, Scale),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
scale_wc <- scale_wc %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
scale_wc$DateObjs <- as.Date(paste0(scale_wc$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
scale_wc$Year <- year(scale_wc$DateObjs)
scale_wc$Month <- month(scale_wc$DateObjs) 
scale_wc <- scale_wc %>% select(-date) #drops date column

#drops NA values 
scale_wc <- scale_wc %>% drop_na(SalesPriceIndex)

#===============================================================================

# Provinces Df (from hpi_type data)

type_provinces <- subset(hpi_type, Region == "Gyeonggi" 
                          | Region == "Gangwon" 
                          | Region == "Chungbuk" | Region == "Chungnam" 
                          | Region == "Gyeongbuk" | Region == "Gyeongnam" 
                          | Region == "Jeju" | Region == "Jeonbuk" 
                          | Region == "Jeonnam")

#changing date columns to rows with corresponding spi values
type_provinces <- type_provinces %>% 
  pivot_longer(cols = -c(Type, Region),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
type_provinces <- type_provinces %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
type_provinces$DateObjs <- as.Date(paste0(type_provinces$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
type_provinces$Year <- year(type_provinces$DateObjs)
type_provinces$Month <- month(type_provinces$DateObjs) 
type_provinces <- type_provinces %>% select(-date) #drops date column

#drops NA values 
type_provinces <- type_provinces %>% drop_na(SalesPriceIndex)

#===============================================================================

# Provinces Df (from hpi_scale data)

scale_provinces <- subset(hpi_scale, Region == "Gyeonggi" 
                          | Region == "Gangwon" 
                          | Region == "Chungbuk" | Region == "Chungnam" 
                          | Region == "Gyeongbuk" | Region == "Gyeongnam" 
                          | Region == "Jeju" | Region == "Jeonbuk" 
                          | Region == "Jeonnam")

#changing date columns to rows with corresponding spi values
scale_provinces <- scale_provinces %>% 
  pivot_longer(cols = -c(Type, Region, Scale),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
scale_provinces <- scale_provinces %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
scale_provinces$DateObjs <- as.Date(paste0(scale_provinces$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
scale_provinces$Year <- year(scale_provinces$DateObjs)
scale_provinces$Month <- month(scale_provinces$DateObjs) 
scale_provinces <- scale_provinces %>% select(-date) #drops date column

#drops NA values 
scale_provinces <- scale_provinces %>% drop_na(SalesPriceIndex)

#===============================================================================

# Seoul Df (from type data)

type_seoul <- subset(hpi_type, grepl("Seoul", Region)) #df for hpi scale df where region = Seoul

#changing date columns to rows with corresponding spi values
type_seoul <- type_seoul %>% 
  pivot_longer(cols = -c(Type, Region),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
type_seoul <- type_seoul %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
type_seoul$DateObjs <- as.Date(paste0(type_seoul$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
type_seoul$Year <- year(type_seoul$DateObjs)
type_seoul$Month <- month(type_seoul$DateObjs) 
type_seoul <- type_seoul %>% select(-date) #drops date column

#drops NA values 
type_seoul <- type_seoul %>% drop_na(SalesPriceIndex)

#===============================================================================

# Seoul Df (from scale data)

scale_seoul <- subset(hpi_scale, grepl("Seoul", Region)) #df for hpi scale df where region = Seoul

#changing date columns to rows with corresponding spi values
scale_seoul <- scale_seoul %>% 
  pivot_longer(cols = -c(Type, Region, Scale),
               names_to = "date",
               values_to = "SalesPriceIndex")

#removes ' Month' from date column string values
scale_seoul <- scale_seoul %>% 
  mutate(date = gsub(" Month", "", date))

#converts date column strings to Date objects 
scale_seoul$DateObjs <- as.Date(paste0(scale_seoul$date, ".01"), format = "%Y.%m.%d")
#creates year & month columns
scale_seoul$Year <- year(scale_seoul$DateObjs)
scale_seoul$Month <- month(scale_seoul$DateObjs) 
scale_seoul <- scale_seoul %>% select(-date) #drops date column

#drops NA values 
scale_seoul <- scale_seoul %>% drop_na(SalesPriceIndex)

#===============================================================================

# Plots (using Whole Country Type subset)
# 
# #plot, subset Total for entire country
# ggplot(data = subset(type_wc, Type == "Total"), 
#        mapping = aes(x = DateObjs, y = SalesPriceIndex)) +
#   geom_line(size = 1) +
#   geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
#   theme(legend.position = c(0.85,0.25)) +
#   guides(fill = "none") + 
#   labs(x = "Date", y = "Sales Price Index", 
#        caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
#        title = "Sales Price Index for All Types of Housing in The Whole Country (2012-01-01 to 2025-02-01)") +
#   theme_minimal()
# 
# #plot, subset Apartments for entire country
# ggplot(data = subset(type_wc, Type == "Apartments"), 
#        mapping = aes(x = DateObjs, y = SalesPriceIndex)) +
#   geom_line(size = 1, color = "firebrick3") +
#   geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
#   theme(legend.position = c(0.85,0.25)) +
#   guides(fill = "none") + 
#   labs(x = "Date", y = "Sales Price Index", 
#        caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
#        title = "Sales Price Index for Apartments in The Whole Country (2012-01-01 to 2025-02-01)")
# 
# #plot, subset Row Houses for entire country
# ggplot(data = subset(type_wc, Type == "Row Houses"), 
#        mapping = aes(x = DateObjs, y = SalesPriceIndex)) +
#   geom_line(size = 1, color = "blue3") +
#   geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
#   theme(legend.position = c(0.85,0.25)) +
#   guides(fill = "none") + 
#   labs(x = "Date", y = "Sales Price Index", 
#        caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
#        title = "Sales Price Index for Row Houses in The Whole Country (2012-01-01 to 2025-02-01)")
# 
# #plot, subset Detached Houses for entire country
# ggplot(data = subset(type_wc, Type == "Detached Houses"), 
#        mapping = aes(x = DateObjs, y = SalesPriceIndex)) +
#   geom_line(size = 1, color = "gray70") +
#   geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
#   theme(legend.position = c(0.85,0.25)) +
#   guides(fill = "none") + 
#   labs(x = "Date", y = "Sales Price Index", 
#        caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
#        title = "Sales Price Index for Detached Houses in The Whole Country (2012-01-01 to 2025-02-01)")

#plot, combining all Types of housing
ggplot(data = type_wc, mapping = aes(x = DateObjs, y = SalesPriceIndex, color = Type)) +
  geom_line(size = 1) +
  geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
  theme(legend.position = c(0.85,0.25)) +
  guides(fill = "none") + 
  labs(x = "Date", y = "Sales Price Index", 
       caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
       title = "Sales Price Index for Types of Housing in The Whole Country (2012-01-01 to 2025-02-01)") +
  theme_minimal()


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

