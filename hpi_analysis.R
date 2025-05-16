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
hpi_type <- hpi_type[-c(229, 459, 135, 365), ] #removes duplicates for jeju, gwangju

#===============================================================================

# Function Creating Subsets From Imported Data 

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

# Function for Creating Plots

create_spi_plot <- function(data_plot, line_colors, aes_mapping, position, graph_title) {
  
  max_spi <- max(data_plot$SalesPriceIndex, na.rm = TRUE)
  
  plot <- ggplot(data = data_plot, 
                 mapping = aes_mapping) +
    geom_line(size = 1) +
    scale_color_manual(values = line_colors) +
    geom_point(x = as.Date("2021-06-01"), y = 100, color = "red") +
    theme_minimal() +
    theme(legend.position = position,
          plot.title = element_text(size = 14, face = "bold"),
          plot.caption = element_text(size = 10),
          axis.title.x = element_text(size = 14),              
          axis.title.y = element_text(size = 14),              
          axis.text.x = element_text(size = 12),               
          axis.text.y = element_text(size = 12),              
          legend.title = element_text(size = 13),              
          legend.text = element_text(size = 11)) +
    guides(fill = "none") + 
    labs(x = "Date", y = "Sales Price Index", 
         caption = "*Red dot indicates value at which Sales Price Index is indexed to: Date = 2021-06-01, SPI = 100", 
         title = graph_title) +
    scale_x_date(breaks = as.Date(paste0(2012:2025, "-01-01")), date_labels = "%Y") +
    scale_y_continuous(breaks = c(seq(0, max_spi-1, by = 5), max_spi), 
                       position = "right", 
                       labels = function(x) ifelse(x == max_spi, paste0("Max: ", x), x)) 
  return(plot)
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

# Gwangju Subset (from type data)
remove_cols <- c("Type", "Region")
filter_region_value <- c('Gwangju')

type_gwangju <- create_subset(input_df = hpi_type, remove_cols, filter_region_value)

# Seoul Region/District Subset (from type data)
remove_cols <- c("Type", "Region")
filter_region_value <- c('Dosim-kwon', 'Dongbuk-kwon', 'Seobuk-kwon', 'Seonam-kwon', 'Dongnam-kwon', 'Jongno-gu',
                         'Jung-gu','Yongsan-gu','Seongdong-gu','Gwangjin-gu','Dongdaemun-gu','Jungnang-gu',
                         'Seongbuk-gu','Dobong-gu','Nowon-gu','Eunpyeong-gu','Seodaemun-gu',
                         'Mapo-gu','Yangcheon-gu','Gangseo-gu','Guro-gu','Geumcheon-gu','Yeongdeungpo-gu',
                         'Dongjak-gu','Gwanak-gu','Seocho-gu','Songpa-gu','Gangdong-gu')
type_seoul_region_dist <- create_subset(input_df = hpi_type, remove_cols, filter_region_value)

#===============================================================================

# Plots (using Whole Country Type subset)

#plot, combining all Types of housing
line_colors <- c("tomato", "seagreen2", "darkturquoise", "slateblue")
data_plot <- type_wholecountry
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Type)
position <- c(0.85,0.25)
graph_title <- "Country-wide Decline in SPI for Apartments & Row Houses From 2022, Detached Houses Unaffected and Continually Trending Upward"

type_wc_plot_all <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_wc_plot_all

#===============================================================================

# Plots (using Provinces Type subset)

#plot, total for housing for every province
line_colors <- c("red3", "orange2", "green2", "blue", "violet", "lightpink", "lightgreen", "dodgerblue", "deeppink2")
data_plot <- subset(type_provinces, Type == "Total")
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Region)
position <- c(0.78,0.3)
graph_title <- "SPI for All Types of Housing for Every Province Lower than 2022-2023 Levels\nGangwon Province Has Highest Current SPI While Gyeonggi Has Lowest"

type_prov_plot_total <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_prov_plot_total

#===============================================================================

# Plots (using Seoul Type subset)

#plot, seoul all house types
line_colors <- c("salmon", "gold", "aquamarine", "darkblue")
data_plot <- subset(type_seoul, Region == "Seoul")
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Type)
position <- c(0.85,0.25)
graph_title <- "SPI For Apartments & Row Houses in Seoul Still Recovering After Decrease From 2022\nDetached Houses in Seoul Unaffected and Continually Trending Upward "

type_seoul_plot_all <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_seoul_plot_all

#===============================================================================

# Plot using Gwangju Type Subset

line_colors <- c("salmon", "gold", "aquamarine", "darkblue")
data_plot <- type_gwangju
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Type)
position <- c(0.85,0.25)
graph_title <- "SPI For Apartments & Row Houses in Gwangju Trending Downwards From 2022, Detached Houses in Gwangju Continually Trending Upward"

type_gwangju_plot_all <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_gwangju_plot_all

#===============================================================================

# Plots (using Seoul Type Region & District Subset)

#plot, seoul regions (동북, 동심, 서북, 서남, 동남) all house types
line_colors <- c("salmon", "gold", "seagreen3", "darkblue", "slateblue2")
data_plot <- subset(type_seoul_region_dist, (Region == "Dongbuk-kwon" | Region == "Dongnam-kwon" | Region == "Dosim-kwon" | Region == "Seobuk-kwon" | Region == "Seonam-kwon") & Type == "Total")
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Region)
position <- c(0.85,0.25)
graph_title <- "SPI For Homes in Dongnam (Southeast) Region of Seoul Has Highest SPI Out of All 5 of Seoul's Regions, Dongbuk (Northeast) Has the Lowest"

type_seoul_regions_plot <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_seoul_regions_plot

#Districts in Dongnam Region plot (excludes Gangnam)
selected_districts <- c('Seocho-gu', 'Songpa-gu', 'Gangdong-gu')
line_colors <- c("salmon", "gold", "seagreen3")
data_plot <- subset(type_seoul_region_dist, Region %in% selected_districts & Type == "Total")
data_plot
aes_mapping <- aes(x = DateObjs, y = SalesPriceIndex, color = Region)
position <- c(0.85,0.25)
graph_title <- "Homes in Seocho District Has Highest SPI Within the Dongnam Region of Seoul (excludes Gangnam District due to lack of data)"

type_seoul_dongnam_plot <- create_spi_plot(data_plot, line_colors, aes_mapping, position, graph_title)
type_seoul_dongnam_plot

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