## ----knit setup, include=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE, cache=TRUE)


## ----data setup, warning=FALSE, message = FALSE-----------------------------------------------------------------------

# Note: this process could take a couple of minutes
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rgbif)) install.packages("rgbif", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(splitstackshape)) install.packages("splitstackshape", repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(randomForest)
library(mice)
library(tidyverse)
library(dslabs)
library(caret)
library(dplyr)
library(rpart.plot)
library("rgbif")
library(corrr)
library(splitstackshape)
library(data.table)



## ----load data--------------------------------------------------------------------------------------------------------

# Load data
#METADATA: http://esapubs.org/archive/ecol/E090/184/metadata.htm

#there are two data sets with overlapping data, so, I am using the larger, WR05 version.
#zooWR93 <- read.table(
  #"http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR93_Aug2008.txt",
  #sep="\t", header=TRUE)

#setwd("D:/RProjects/Zoo/")
reload=TRUE
if(reload){
  zoo <- read.table("PanTHERIA_1-0_WR05_Aug2008.txt", sep="\t", header=TRUE)

}

#replace -999 with NA for correct handling in R
#see missing data section for more details on missing data handling
zoo[zoo==-999]<-NA
totalrows <- nrow(zoo)



## ----get vernacular names, fig.width=12,fig.height=8------------------------------------------------------------------
#function to return a vernacular/common name for each species from GBIF
#Note that this can't run inside a loop or DPLYR mutate- it will cause a bad request error. Names may ONLY be looked up one at a time.

getvernacularname <- function(x){
speckey <- rgbif::name_backbone(name=x)$speciesKey
speckey <- as.numeric(speckey)
vernac <- rgbif::name_usage(key=speckey, data='vernacularNames')
vnames <- vernac[[2]] %>% 
  filter(language=='eng') %>% 
  select(vernacularName)
vx <- unique(vnames)
vname <- vx[which.max(tabulate(match(vnames, vx)))] %>% 
  slice(1:1) %>% 
  pull(vernacularName)
vname
}

df <- data.frame()

#example usage
lookupbinomial <- 'Balaenoptera musculus'
vernac <- getvernacularname(lookupbinomial)
lookup <- data.frame(lookup=paste('The common name for',lookupbinomial,'is',vernac))
df <- df %>% bind_rows(lookup)

lookupbinomial <- 'Rattus rattus'
vernac <- getvernacularname(lookupbinomial)
lookup <- data.frame(lookup=paste('The common name for',lookupbinomial,'is',vernac))
df <- df %>% bind_rows(lookup)

lookupbinomial <- 'Canis lupus'
vernac <- getvernacularname(lookupbinomial)
lookup <- data.frame(lookup=paste('The common name for',lookupbinomial,'is',vernac))
df <- df %>% bind_rows(lookup)
df %>% knitr::kable()



## ----order index------------------------------------------------------------------------------------------------------

order_info <-data.frame()
order_desc <- data.frame(taxo_order='Afrosoricida',taxo_order_desc='Tenrecs')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Artiodactyla',taxo_order_desc='Even-toed ungulates')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Carnivora',taxo_order_desc='Bears & Wolves')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Cetacea',taxo_order_desc='Whales & Dolphins')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Chiroptera',taxo_order_desc='Bats')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Cingulata',taxo_order_desc='Armadillos & Anteaters')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Dasyuromorphia',taxo_order_desc='Carnivorous Marsupials')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Dermoptera',taxo_order_desc='Flying Lemurs')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Didelphimorphia',taxo_order_desc='Opossums')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Diprotodontia',taxo_order_desc='Herbivorous Marsupials')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Erinaceomorpha',taxo_order_desc='Hedgehogs')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Hyracoidea',taxo_order_desc='Hydraxes')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Lagomorpha',taxo_order_desc='Rabbits & Hares')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Macroscelidea',taxo_order_desc='Elephant Shrews')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Microbiotheria',taxo_order_desc='Monito del montes')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Monotremata',taxo_order_desc='Platypus & Echidnas')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Notoryctemorphia',taxo_order_desc='Marsupial Mole')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Paucituberculata',taxo_order_desc='Shrew Opossums')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Peramelemorphia',taxo_order_desc='Bandicoots')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Perissodactyla',taxo_order_desc='Odd-toed Ungulates')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Pholidota',taxo_order_desc='Pangolins')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Pilosa',taxo_order_desc='Sloths & Anteaters')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Primates',taxo_order_desc='Apes & Monkeys')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Proboscidea',taxo_order_desc='Elephants')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Rodentia',taxo_order_desc='Rats & Mice')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Scandentia',taxo_order_desc='Treeshrews')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Sirenia',taxo_order_desc='Manatees & Dugongs')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Soricomorpha',taxo_order_desc='Moles & Shrews')
order_info <- order_info %>% bind_rows(order_desc)
order_desc <- data.frame(taxo_order='Tubulidentata',taxo_order_desc='Aardvarks')
order_info <- order_info %>% bind_rows(order_desc)

order_info <- order_info %>% mutate(taxo_order = as.factor(taxo_order))
order_info <- order_info %>% mutate(taxo_order_desc = as.factor(taxo_order_desc))

order_info %>% slice(1:10) %>% knitr::kable()


## ----data wrangling---------------------------------------------------------------------------------------------------
#create a subset of teh original data with the variables we are interested in, with the correct data type formats
tinyzoo <- zoo %>% 
  dplyr::select(
  taxo_order = MSW05_Order,
  taxo_family = MSW05_Family,
  taxo_genus = MSW05_Genus,
  taxo_species = MSW05_Species,
  headlen_mm=X13.1_AdultHeadBodyLen_mm,
  forearmlen_mm=X8.1_AdultForearmLen_mm,
  mass_grams=X5.1_AdultBodyMass_g,
  mass_grams2=X5.5_AdultBodyMass_g_EXT,
  litter_size=X15.1_LitterSize,
  litters_peryear=X16.1_LittersPerYear,
  litters_peryear2=X16.2_LittersPerYear_EXT,
  gestation_days= X9.1_GestationLen_d,
  longevity_months= X17.1_MaxLongevity_m,
  diet_breadth=X6.1_DietBreadth,
  pop_density= X21.1_PopulationDensity_n.km2,
  trophic_level= X6.2_TrophicLevel,
  terrestriality= X12.2_Terrestriality,
  sex_maturity= X23.1_SexualMaturityAge_d,
  wean_age= X25.1_WeaningAge_d,
  activity_cycle= X1.1_ActivityCycle,
  range_km2 = X26.1_GR_Area_km2,
  max_lat = X26.2_GR_MaxLat_dd,
  min_lat = X26.3_GR_MinLat_dd,
  max_lng =X26.5_GR_MaxLong_dd,
  min_lng =X26.6_GR_MinLong_dd,
  mid_lat = X26.4_GR_MidRangeLat_dd,
  mid_lng =X26.7_GR_MidRangeLong_dd,
  references=References
  ) %>%
 mutate(
    data_set = 'PanTHERIA_WR05_Aug2008.txt',
    taxo_class = as.factor('Mammalia'),
    taxo_order = as.factor(taxo_order),
    taxo_family = as.factor(taxo_family),
    taxo_genus = as.factor(taxo_genus),
    taxo_species = as.factor(taxo_species),
    taxo_binomial = as.factor(paste(taxo_genus, taxo_species)),
    taxo_order_fam = as.factor(paste(taxo_order,'>', taxo_family)),
    taxo_order_fam_genus = as.factor(paste(taxo_order,'>', taxo_family,'>', taxo_genus)),
    taxo_all = as.factor(paste(taxo_class,'>',taxo_order,'>', taxo_family,'>', taxo_genus, '>', taxo_species)),
    headlen_mm = as.numeric(headlen_mm),
    forearmlen_mm = as.numeric(forearmlen_mm),
    mass_grams = as.numeric(coalesce(mass_grams,mass_grams2)),
    litter_size = as.numeric(litter_size),
    litters_peryear = as.numeric(coalesce(litters_peryear,litters_peryear2)),
    gestation_days = as.numeric(gestation_days),
    longevity_months = as.numeric(longevity_months),
    diet_breadth=as.numeric(diet_breadth),
    pop_density= as.numeric(pop_density),
    trophic_level= as.numeric(trophic_level),
    terrestriality= as.numeric(terrestriality),
    sex_maturity= as.numeric(sex_maturity),
    activity_cycle= as.numeric(activity_cycle),
    range_km2 = as.numeric(range_km2),
    max_lat = as.numeric(max_lat),
    min_lat = as.numeric(min_lat),
    max_lng = as.numeric(max_lng),
    min_lng = as.numeric(min_lng),
    mid_lat = as.numeric(mid_lat),
    mid_lng = as.numeric(mid_lng),
    wean_age= as.numeric(wean_age),
    marine = ifelse(taxo_order == 'Cetacea' | taxo_order == 'Sirenia',1,0),
    flying = ifelse(taxo_order == 'Chiroptera',1,0),
    )   %>% 
  left_join(order_info, by="taxo_order") %>% 
    select(data_set,
    taxo_class,
    taxo_order,
    taxo_order_desc,
    taxo_family,
    taxo_genus,
    taxo_order_fam,
    taxo_order_fam_genus,
    taxo_species,
    taxo_binomial,
    taxo_all,
    references,
    headlen_mm,
    forearmlen_mm,
    mass_grams,
    litter_size,
    litters_peryear,
    gestation_days,
    longevity_months,
    diet_breadth,
    pop_density,
    trophic_level,
    terrestriality,
    sex_maturity,
    wean_age,
    activity_cycle,
    range_km2,
    mid_lat,
    mid_lng,
    max_lat,
    min_lat,
    max_lng,
    min_lng,
    marine,
    flying
    ) %>% 
   arrange(taxo_order,taxo_family,taxo_genus,taxo_species)



## ----map orders, fig.width=16,fig.height=8----------------------------------------------------------------------------

world <- map_data("world") 
mapdata <- tinyzoo %>% mutate(order_info = paste(taxo_order,"\n",taxo_order_desc,"\n"))

wmap <- ggplot(world, aes(long, lat)) + 
  geom_point(size = .1, show.legend = FALSE) +
  geom_point(data = mapdata, alpha=.7, 
                                           mapping = aes(
                                              size = range_km2, 
                                         x = mid_lng, 
                                           y = mid_lat, 
                                           colour = order_info)) +
  coord_quickmap() +
  labs(title = "All Orders Geographic Distributions",
       caption = "Data source: Pantheria")
ggsave("worldmap.png",plot = wmap,width=16,height=8)



## ---- fig.width=14,fig.height=12--------------------------------------------------------------------------------------
rmap <- ggplot(world, aes(long, lat)) + 
  geom_point(size = .1, show.legend = FALSE) +
  geom_point(data = tinyzoo %>% filter(taxo_order %in% c('Afrosoricida','Chiroptera','Primates','Lagomorpha','Rodentia','Carnivora','Proboscidea','Diprotodontia')) %>% mutate(order_info = paste(taxo_order,"\n",taxo_order_desc,"\n")), alpha=.7, 
                                           mapping = aes(
                                              size = range_km2, 
                                         x = mid_lng, 
                                           y = mid_lat, 
                                           colour = order_info)) +
  coord_quickmap() + facet_wrap(taxo_order ~ ., ncol = 2) +
  labs(title = "Selected Orders Geographic Distributions",
       caption = "Data source: Pantheria")

ggsave("regionmap.png",plot = rmap,width=14,height=12)



## ----missing values list, fig.width=12,fig.height=8-------------------------------------------------------------------

#review how many rows have NA values
missing_list <- data.frame(
headlen_mm_missing = sum(is.na(tinyzoo$headlen_mm)),
forearmlen_mm_missing = sum(is.na(tinyzoo$forearmlen_mm)),
mass_grams_missing = sum(is.na(tinyzoo$mass_grams)),
litter_size_missing = sum(is.na(tinyzoo$litter_size)),
litters_peryear_missing = sum(is.na(tinyzoo$litters_peryear)),
gestation_days_missing = sum(is.na(tinyzoo$gestation_days)),
longevity_months_missing = sum(is.na(tinyzoo$longevity_months)),
diet_breadth_missing = sum(is.na(tinyzoo$diet_breadth)),
pop_density_missing = sum(is.na(tinyzoo$pop_density)),
trophic_level_missing = sum(is.na(tinyzoo$trophic_level)),
terrestriality_missing = sum(is.na(tinyzoo$terrestriality)),
sex_maturity_missing = sum(is.na(tinyzoo$sex_maturity)),
wean_age_missing = sum(is.na(tinyzoo$wean_age)),
range_km2_missing = sum(is.na(tinyzoo$range_km2)),
mid_lat_missing = sum(is.na(tinyzoo$mid_lat)),
mid_lng_missing = sum(is.na(tinyzoo$mid_lng))
)
missing_list = t(missing_list)
missing_list %>% knitr::kable()


## ----missing values, fig.width=12,fig.height=8------------------------------------------------------------------------
#prepare data for a heatmap grid
missingpcts <- tinyzoo %>% 
  select(-taxo_family,-taxo_genus,-taxo_species,-taxo_binomial, -taxo_class,-taxo_order_desc,-taxo_order_fam,-taxo_order_fam_genus,-taxo_all) %>% 
  group_by(taxo_order) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))/n()))

row.names(missingpcts) <- missingpcts$taxo_order

heatmapdata <- missingpcts %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)

r <- 1

#render grid & annotations
ggplot(heatmapdata, aes(x = rowname, y = colname, alpha = value)) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="none") +
  annotate("path",color="red",
   x=5+r*cos(seq(0,2*pi,length.out=100)),
   y=5+r*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=10,y=5,color="red",label="Only Chiroptera have forearm length") +
  annotate("path",color="red",
   x=4+r*cos(seq(0,2*pi,length.out=100)),
   y=17+5*sin(seq(0,2*pi,length.out=100))) +
  annotate("path",color="red",
   x=27+r*cos(seq(0,2*pi,length.out=100)),
   y=17+5*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=10,y=20,color="red",label="Cetacea + Sirenia missing geographic fields") +
  labs(title = "Missing Data by Order",
       caption = "Data source: Pantheria")


## ----complete rows, warning=FALSE,fig.width=12,fig.height=8-----------------------------------------------------------
#review how many rows have ALL nonnegative values
#this would limit the number of possible matches if we use a complete case apporoach
complete_rows <- tinyzoo %>% filter(
     headlen_mm > 0 & 
     mass_grams > 0 & 
     activity_cycle > 0 & 
     litter_size > 0 & 
     litters_peryear > 0 & 
     gestation_days > 0 &
     longevity_months > 0 & 
     diet_breadth > 0 & 
     pop_density > 0 & 
     trophic_level > 0 &
     sex_maturity > 0 &
     wean_age > 0 
     )
complete_count <- nrow(complete_rows)



## ----empty rows, warning=FALSE,fig.width=12,fig.height=8--------------------------------------------------------------
#find rows that have no predictor values
empty_rows <- tinyzoo %>% filter(
      is.na(headlen_mm) & 
      is.na(mass_grams) & 
      is.na(litter_size) & 
      is.na(litters_peryear) & 
      is.na(gestation_days) &
      is.na(longevity_months) & 
      is.na(diet_breadth) & 
      is.na(trophic_level) &
      is.na(pop_density) &
      is.na(sex_maturity) &
      is.na(wean_age) &
      is.na(activity_cycle) 
     
)
#record number of empty rows
empty_count <- nrow(empty_rows)


## ---------------------------------------------------------------------------------------------------------------------

#remove empty rows from tinyzoo
tinyzoo <- tinyzoo %>% anti_join(empty_rows,by="taxo_binomial")
tinyzoo$taxo_order <- factor(tinyzoo$taxo_order) 
tinyzoo$taxo_order <- droplevels(tinyzoo$taxo_order)



## ----mice, message=FALSE,warning=FALSE,echo=FALSE,fig.width=6,fig.height=12-------------------------------------------
library(mice)

#make a dataframe with just the predictors
predictors_only <- tinyzoo %>% select(
     headlen_mm, 
     forearmlen_mm, 
     mass_grams, 
     litter_size, 
     litters_peryear, 
     gestation_days,
     longevity_months,
     diet_breadth, 
     pop_density, 
     trophic_level,
     terrestriality,
     sex_maturity,
     wean_age,
     activity_cycle,
     range_km2,
     mid_lat,
     mid_lng
    )


##Inspect missingness of first 100 rows in the training adata
missing <- md.pattern(predictors_only %>% sample_n(100),rotate.names=TRUE,plot=TRUE) 
#missing

#missingness <- md.pattern(predictors_only,rotate.names=TRUE,plot=FALSE) 
#missingness




## ----examine order data points, fig.width=12,fig.height=6-------------------------------------------------------------
plot_ord <- tinyzoo %>% 
  group_by(taxo_order) %>% 
  dplyr::summarize(group_count =  n()) %>% 
  ggplot(aes(x=taxo_order,y=group_count, fill=taxo_order)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="none") +
  labs(title = "Number of Species per Order",
       caption = "Data source: Pantheria")
plot_ord


## ---- fig.width=12,fig.height=12--------------------------------------------------------------------------------------
all_ord <- tinyzoo %>% group_by(taxo_order) %>% 
  dplyr::summarize(group_count =  n()) 

median_group_count <- median(all_ord$group_count)

minority_ord <- tinyzoo %>% group_by(taxo_order) %>% 
  dplyr::summarize(group_count =  n()) %>% 
  filter(group_count < median_group_count) 

minority_groups <- nrow(minority_ord)



## ----examine family, fig.width=16,fig.height=6------------------------------------------------------------------------
#create families plot
plot_fam <- tinyzoo %>% filter(taxo_order %in% c('Rodentia','Chiroptera','Primates')) %>%
  group_by(taxo_order, taxo_order_fam) %>% 
  dplyr::summarize(group_count =  n()) %>% 
  ggplot(aes(x=taxo_order_fam,y=group_count, fill=taxo_order)) + 
  geom_col() + 
  facet_wrap(taxo_order ~ ., scales = "free") + 
  theme(axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="none") +
  labs(title = "Number of Species per Family - Orders: Chiroptera, Primates, Rodentia",
       caption = "Data source: Pantheria")
plot_fam


## ----examine family 3, fig.width=12,fig.height=12---------------------------------------------------------------------
#table of group counts
all_fam <- tinyzoo %>% 
  group_by(taxo_order_fam) %>% 
  dplyr::summarize(group_count =  n()) 



#med group count
median_fam_count <- median(all_fam$group_count)

#select minority classes
minority_fam <- tinyzoo %>% 
  group_by(taxo_order_fam) %>% 
  dplyr::summarize(group_count =  n()) %>% 
  filter(group_count < median_fam_count) %>% 
  mutate(minority = TRUE) %>% 
  ungroup()






## ----examine family 2, fig.width=16,fig.height=6----------------------------------------------------------------------
#create families plot
plot_fam <- tinyzoo %>% filter(taxo_order == 'Chiroptera' & taxo_family == 'Vespertilionidae') %>%
  group_by(taxo_order, taxo_genus) %>% 
  dplyr::summarize(group_count =  n()) %>% 
  ggplot(aes(x=taxo_genus,y=group_count, fill=taxo_genus)) + 
  geom_col() + 
  facet_wrap(taxo_order ~ ., scales = "free") + 
  theme(axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="none") +
  labs(title = "Number of Species per Genus - Order: Chiroptera - Family: Vespertilionidae",
       caption = "Data source: Pantheria")
plot_fam


## ---------------------------------------------------------------------------------------------------------------------
#create a table of all genus types for reference
all_genus <- tinyzoo %>% 
  group_by(taxo_order_fam_genus) %>% 
  dplyr::summarize(group_count =  n()) 


## ----getminmax, warning=FALSE, message=FALSE,  fig.width=12,fig.height=8----------------------------------------------

#getminmax function to compare species with min/max values for a given metric
getminmax <- function(data, col) {
  
  topspecies <- data %>% 
    arrange(desc(!!sym(col))) %>% 
    slice(1:1) %>% 
    mutate(species_binom = paste(taxo_genus,taxo_species)) %>% 
    pull(species_binom)
  
  botspecies <- data %>% 
    arrange(!!sym(col)) %>% 
    slice(1:1) %>% 
    mutate(species_binom = paste(taxo_genus,taxo_species)) %>% 
    pull(species_binom)
  
  topstat <- data %>% 
    arrange(desc(!!sym(col))) %>% 
    slice(1:1) %>% 
    mutate(species_binom = paste(taxo_genus,taxo_species)) %>% 
    pull(sym(col))
  
  botstat <- data %>% 
    arrange(!!sym(col)) %>% 
    slice(1:1) %>% 
    mutate(species_binom = paste(taxo_genus,taxo_species)) %>% 
    pull(sym(col))

  checkrow <- data.frame(
                      stat=col,
                      #maxspecies=topspecies,
                      maxspecies_vernacular=getvernacularname(topspecies),
                      maxstat=topstat,
                      #minspecies=botspecies,
                      minspecies_vernacular=getvernacularname(botspecies),
                      minstat=botstat
                   )
   
  checkrow
}


spectable <- data.frame()

checkrow <- getminmax(tinyzoo,c("mass_grams"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("headlen_mm"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo %>% filter(flying == 1),c("forearmlen_mm"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("litter_size"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("litters_peryear"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("longevity_months"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("gestation_days"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("wean_age"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("pop_density"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("trophic_level"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("terrestriality"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("diet_breadth"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("activity_cycle"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("range_km2"))
spectable <- spectable %>% bind_rows(checkrow)

checkrow <- getminmax(tinyzoo,c("mid_lat"))
spectable <- spectable %>% bind_rows(checkrow)



spectable  %>% knitr::kable()



## ----viz orders, fig.width=12,fig.height=12---------------------------------------------------------------------------
# Plot distribution of each predictor stratified by order
tinyzoolog <- tinyzoo %>% mutate(mass_grams_log10 = log10(mass_grams),
                         headlen_mm_log10 = log10(headlen_mm),
                         pop_density_log10 = log10(pop_density))

#create some log10 transforms
tinyzoolog %>% gather(varz, length, 
                mass_grams_log10,
                headlen_mm_log10,
                litter_size,
                litters_peryear,
                gestation_days,
                longevity_months,
                diet_breadth,
                pop_density_log10,
                trophic_level,
                terrestriality,
                sex_maturity,
                wean_age,
                 activity_cycle,
     range_km2,
     mid_lat,
     mid_lng
                ) %>%
  ggplot(aes(paste(taxo_order), 
             length, 
             fill = paste(taxo_order))) +
  geom_boxplot() +
  facet_wrap(~varz, scales = "free", ncol=2) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position="bottom",axis.text.x = element_blank())  + 
  labs(title = "Distributions of Physical, Behavioral and Reproductive Characteristics", subtitle = "Distribution of median observed values for each species grouped by order", caption = "Data source: Pantheria")



## ----viz families, fig.width=12,fig.height=12-------------------------------------------------------------------------
# Plot distribution of each predictor stratified by family
tinyzoolog %>% filter(taxo_order == 'Rodentia') %>% gather(varz, length, 
                mass_grams_log10,
                headlen_mm_log10,
                litter_size,
                litters_peryear,
                gestation_days,
                longevity_months,
                diet_breadth,
                pop_density_log10,
                trophic_level,
                terrestriality,
                sex_maturity,
                wean_age,  
                activity_cycle,
     range_km2,
     mid_lat,
     mid_lng) %>%
  ggplot(aes(paste(taxo_family), 
             length, 
             fill = paste(taxo_family))) +
  geom_boxplot() +
  facet_wrap(~varz, scales = "free", ncol=2) +
  theme(legend.position="bottom",axis.text.x = element_blank()) + 
  labs(title = "Distributions of Physical, Behavioral and Reproductive Characteristics - Order: Rodentia", subtitle = "Distribution of median observed values for each species grouped by family", caption = "Data source: Pantheria")



## ----viz genus, fig.width=12,fig.height=12----------------------------------------------------------------------------
# Plot distribution of each predictor stratified by family
tinyzoolog %>% filter(taxo_order == 'Rodentia' & taxo_family == 'Sciuridae') %>% gather(varz, length, 
                mass_grams_log10,
                headlen_mm_log10,
                litter_size,
                litters_peryear,
                gestation_days,
                longevity_months,
                diet_breadth,
                pop_density_log10,
                trophic_level,
                terrestriality,
                sex_maturity,
                wean_age,  
                activity_cycle,
     range_km2,
     mid_lat,
     mid_lng) %>%
  ggplot(aes(paste(taxo_genus), 
             length, 
             fill = paste(taxo_genus))) +
  geom_boxplot() +
  facet_wrap(~varz, scales = "free", ncol=2) +
  theme(legend.position="bottom",axis.text.x = element_blank()) + 
  labs(title = "Distributions of Physical, Behavioral and Reproductive Characteristics - Order: Rodentia, Family:  Sciuridae", subtitle = "Distribution of median observed values for each species grouped by genus", caption = "Data source: Pantheria")



## ----examine correlations, fig.width=12,fig.height=8------------------------------------------------------------------
library(corrr)
library(gridExtra)
library(grid)

#create a dataset with just the predictors
tzvars <- tinyzoo %>% select(mass_grams,headlen_mm,litter_size,litters_peryear,gestation_days,wean_age,sex_maturity,longevity_months,pop_density,diet_breadth,terrestriality,trophic_level,activity_cycle,range_km2,mid_lat,mid_lng)

cor_tbl <- corrr::correlate(tzvars)

cor_tbl


## ----getpointplot, fig.width=12,fig.height=16-------------------------------------------------------------------------

getpointplot <- function(data, col1, col2, colgroup, loglist, opac, desc) {
 hull <- data %>%
  filter(!is.na(!!sym(col1)) & !is.na(!!sym(col2))) %>%  
  mutate(loggroup = round(log10(!!sym(col1)))) %>%  
  filter(loggroup %in% loglist) %>%  
  group_by(!!sym(colgroup)) %>%
  slice(chull(!!sym(col1), !!sym(col2)))
  
 center <- data %>%
  filter(!is.na(!!sym(col1)) & !is.na(!!sym(col2))) %>%  
  mutate(loggroup = round(log10(!!sym(col1)))) %>%  
  filter(loggroup %in% loglist) %>%  
  group_by(!!sym(colgroup)) %>%
  summarize(avgx = mean(!!sym(col1)),avgy = mean(!!sym(col2)))
 
   p1 <- data %>% 
  filter(!is.na(!!sym(col1)) & !is.na(!!sym(col2))) %>%  
  mutate(loggroup = round(log10(!!sym(col1)))) %>%  
  filter(loggroup %in% loglist) %>%  
  ggplot(aes(!!sym(col1), !!sym(col2), color = !!sym(colgroup), fill = !!sym(colgroup))) + 
  geom_point()  + 
  geom_polygon(data = hull, alpha = opac) + 
  geom_label(data = center, aes(x=avgx,y=avgy,label=!!sym(colgroup)),fill='#FFFFFF')  + 
 #facet_wrap(~loggroup, scales = "free",ncol=2) +
  theme(legend.position="none") + 
  labs(title = paste("Correlated Characteristics: ",col1," and ",col2), 
       subtitle = paste("Distribution of median observed values: ",desc), 
       caption = "Data source: Pantheria")

   
     p1
  
  
  
}

#reproductive characteristics
p1 <- getpointplot(tinyzoo,'sex_maturity','wean_age','taxo_order_desc', c(1),0.05,"Very Fast Maturity")
p2 <- getpointplot(tinyzoo,'sex_maturity','wean_age','taxo_order_desc', c(2),0.05,"Fast Maturity")
p3 <- getpointplot(tinyzoo,'sex_maturity','wean_age','taxo_order_desc', c(3),0.05,"Average Maturity")
p4 <- getpointplot(tinyzoo,'sex_maturity','wean_age','taxo_order_desc', c(4),0.05,"Slow Maturity")
grid.arrange(p1, p2, p3, p4,nrow = 2)

#physical characteristics
p1 <- getpointplot(tinyzoo,'mass_grams','headlen_mm','taxo_order_desc', c(1,2),0.05,"Small species")
p2 <- getpointplot(tinyzoo,'mass_grams','headlen_mm','taxo_order_desc', c(3,4),0.05,"Medium species")
p3 <- getpointplot(tinyzoo,'mass_grams','headlen_mm','taxo_order_desc', c(5,6),0.05,"Large species")
p4 <- getpointplot(tinyzoo,'mass_grams','headlen_mm','taxo_order_desc', c(7,8),0.05,"Very Large species")
grid.arrange(p1, p2, p3, p4, nrow = 2)




## ----rpart, fig.width=12,fig.height=12--------------------------------------------------------------------------------
library(rpart)
library(caret)
#reduce the data to the predictor fields that will be used in the models
tzNA_pred_species <- tinyzoo %>% select(
              taxo_species,
              taxo_genus,
              taxo_order,
              taxo_family, 
              mass_grams,
              forearmlen_mm,
              headlen_mm,
              litter_size,
              litters_peryear,
              gestation_days,
              longevity_months,
              diet_breadth,
              pop_density,
              trophic_level,
              terrestriality,
              sex_maturity,
              wean_age,
              activity_cycle,
              range_km2,
              mid_lat,
              mid_lng
                ) %>% mutate(taxo_species = as.factor(taxo_species),
                             taxo_genus = as.factor(taxo_genus),
                             taxo_order = as.factor(taxo_order),
                             taxo_family = as.factor(taxo_family))







## ----getcmplot function-----------------------------------------------------------------------------------------------
#this function will build a visualization grid plot of the confusion matrix that we will use for analysis of each model.
#Note: this chart sample was adapted from a stackoverflow post.
getcmplot <- function(cm,name,subtitle='') {
 
cm_tbl <- data.frame(cm$table)

# add mutations to the CM visulaization
plot_tbl <- cm_tbl %>%
  mutate(goodbad = ifelse(cm_tbl$Prediction == cm_tbl$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = ifelse(Freq+sum(Freq)>1,Freq/sum(Freq),0) )

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plot_tbl, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Freq > 0, Freq, "")), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlim(rev(levels(plot_tbl$Reference))) + 
  labs(title = paste("Confusion Matrix: ",name), 
       subtitle = subtitle, 
       caption = "Data source: Pantheria")
  
  
}


## ----measuremodel function--------------------------------------------------------------------------------------------
#this function will measure the accuracy, mean F1 score and number of class predictions made
measuremodel <- function(cm,name,ds,level){
  
overall_accuracy <- cm$overall["Accuracy"]  
orders_predicted <- 0
orders_actual <- nrow(cm$byClass)
xsum <- 0
xrows <- seq(from = 1, to = nrow(cm$byClass), by = 1)
for (x in xrows){
  if(!is.na(cm$byClass[x,7])){
    xsum <- xsum + cm$byClass[x,7]
    orders_predicted <- orders_predicted + 1
  }
}

F1_mean <- xsum/orders_predicted

results_df <- data.frame(
model=name,
dataset=ds,
Predicted=orders_predicted,
Accuracy=mean(overall_accuracy),
F1_Mean=F1_mean)

results_df
}




## ----partitions-------------------------------------------------------------------------------------------------------
##--------------------------------------

createstratifiedpartitions <- function(dataset,groupedby,pct,setseed){

  set.seed(setseed, sample.kind = "Rounding")

  # Take a 50% sample from all -A- groups in DF
  z_strat <- stratified(dataset, groupedby, pct, bothSets = TRUE)
  
  #retrieve the two sets
  z_train <- z_strat$SAMP1
  z_temp <- z_strat$SAMP2
  
  # Make sure vals in validation set are also in training set
  z_test <- z_temp %>%
  semi_join(z_train,
            by=groupedby)

  # Add rows removed from validation set back into main set
  z_removed <- anti_join(z_temp, z_test)
  z_train <- rbind(z_train, z_removed)

  
  returnobj <-list(
                  strat=z_strat,          
                  trainDS=z_train,
                  testDS=z_test
                  )


  class(returnobj) <- "resulttable"
  returnobj

}
#first stratify into a 10% holdout for validating the model 
#this will try to populate as many different genuses in the validaton data as possible.
tzNA_species_holdout <- createstratifiedpartitions(tzNA_pred_species, "taxo_genus", .9,1)

#take the other 90% and split that into a 80% training/20% test partition
tzNA_species_working <- tzNA_species_holdout$trainDS

#create a dataset of 20% of the working partition
tzNA_order_parts <- createstratifiedpartitions(tzNA_species_working, "taxo_order", .8,1)

#set some variables for display
originalrows <- nrow(tzNA_pred_species)

holdoutrows <- nrow(tzNA_species_holdout$testDS)
trainingrows <- nrow(tzNA_species_holdout$trainDS)

trainingfold <- nrow(tzNA_order_parts$trainDS)
testfold <- nrow(tzNA_order_parts$testDS)

count_orders <- nrow(tzNA_order_parts$testDS %>% group_by(taxo_order) %>% summarize(group_count = n()))





## ----expected value---------------------------------------------------------------------------------------------------
ev_order <- round(1/nrow(all_ord),3)
ev_fam <- round(1/nrow(all_fam),4)
ev_genus <- round(1/nrow(all_genus),5)



## ----baseline---------------------------------------------------------------------------------------------------------
#we will start with the tzNA table, on the order level
tz_train <- tzNA_order_parts$trainDS
tz_test <- tzNA_order_parts$testDS
##--------------------------------------
predictions <- rep(as.factor('Rodentia'), nrow(tz_test))

tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% 
  pull(taxo_order)

#get a confusion matrrix with the predictions vs actuals
cm <- confusionMatrix(data=predictions,reference=tz_actual)

measure_df0 <- measuremodel(cm,"1- Baseline","With 0's","Order")
measure_df_all <- measure_df0
measure_df_all %>% knitr::kable()



## ----check results 1 cm, fig.width=16,fig.height=16-------------------------------------------------------------------

r = .8
cmplot <- getcmplot(cm,"Baseline") 
cmplot + annotate("path",color="blue",
   x=17+ r*cos(seq(0,2*pi,length.out=100)),
   y=25+ r*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=17,y=24,color="blue",label="Lagomorpha being incorrectly classified as Rodentia")  + 
   annotate("path",color="blue",
   x=5+ r*cos(seq(0,2*pi,length.out=100)),
   y=25+ r*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=5,y=24,color="blue",label="Rodentia being correctly classified") 


## ----CART model-------------------------------------------------------------------------------------------------------
#we will start with the tzNA table, on the order level
tz_train <- tzNA_order_parts$trainDS
tz_test <- tzNA_order_parts$testDS


## ----fit order--------------------------------------------------------------------------------------------------------
##--------------------------------------
#override rparts complexity parameter to -1 to demonstrate an unpruned tree
tz_train <- tz_train %>% select(-taxo_family,-taxo_genus,-taxo_species)
set.seed(111, sample.kind = "Rounding")

fit_order <- rpart(taxo_order ~ .,
                   data = tz_train,
                   minsplit = 2,
                   minbucket = 1,
                   maxdepth =8,
                   cp = -1,
                   method = "class")



## ----predict with rpart, fig.width=8,fig.height=10--------------------------------------------------------------------
##--------------------------------------
#prx is teh prediction model
prx <- predict(object=fit_order,tz_test[-1],type="class")

#this is th actual orders extracted from the test set we just got predictions for
tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)

#get a confusion matrrix with the predictions vs actuals
cm <- confusionMatrix(data=prx,reference=tz_actual)

measure_df1 <- measuremodel(cm,"2- CART - Unpruned","With 0's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df1)
measure_df_all %>% knitr::kable()


## ----check results 2 cm, fig.width=16,fig.height=16-------------------------------------------------------------------
cmplot <- getcmplot(cm,"RPart - Classification Tree - Unpruned")
cmplot + 
  annotate("path",color="blue",
   x=17+ r*cos(seq(0,2*pi,length.out=100)),
   y=25+ r*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=17,y=24,color="blue",label="Lagomorpha being incorrectly classified as Rodentia")  + 
   
  annotate("path",color="blue",
   x=17+ r*cos(seq(0,2*pi,length.out=100)),
   y=13+ r*sin(seq(0,2*pi,length.out=100))) +
  
  annotate("text",x=17,y=12,color="blue",label="Lagomorpha being correctly classified")  + 
  
  annotate("path",color="blue",
   x=5+ r*cos(seq(0,2*pi,length.out=100)),
   y=25+ r*sin(seq(0,2*pi,length.out=100))) +
  annotate("text",x=4,y=24,color="blue",label="Rodentia being correctly classified") 


## ----plot rpart decisions, fig.width=16,fig.height=14-----------------------------------------------------------------
##--------------------------------------
# use rpart to improve display

rpart.plot(fit_order, 
           box.palette="RdBu", 
           shadow.col="gray", 
           extra = 0, 
           fallen.leaves= FALSE, 
           cex = .5,
           Margin=.1, 
           legend.x = -0.15, 
           legend.y = 1,
           compress=FALSE,
           main="Unpruned Tree Diagram - Too Complex")



## ---------------------------------------------------------------------------------------------------------------------
plotcp(fit_order)


## ----prunedfit, fig.width=16,fig.height=16----------------------------------------------------------------------------
# prune the tree 
pruned_fit <- prune(fit_order, cp = 0.002)
rpart.plot(pruned_fit, 
           box.palette="RdBu", 
           shadow.col="gray", 
           extra = 0, 
           fallen.leaves= FALSE, 
           cex = .5,
           Margin=.4, 
           legend.x = -0.5, 
           legend.y = 1,
           compress=FALSE,
           main='Pruned Tree')


## ----predict pruned, fig.width=16,fig.height=16-----------------------------------------------------------------------
##--------------------------------------

prx <- predict(object=pruned_fit,tz_test[-1],type="class")

tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
cm <- confusionMatrix(data=prx,reference=tz_actual)
pruned_acc <- cm$overall["Accuracy"]

##--------------------------------------

measure_df2 <- measuremodel(cm,"3- CART - Pruned","With 0's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df2)
measure_df_all %>% knitr::kable()



## ----check results 4, fig.width=16,fig.height=16----------------------------------------------------------------------
getcmplot(cm,"RPart - Classification Tree - Pruned")


## ----tryrandom, ECHO = TRUE-------------------------------------------------------------------------------------------
set.seed(2, sample.kind = "Rounding")
tryrow <- sample_n(tz_test, 1)

correct_order <- tryrow %>% select(taxo_order)
print(as.character(correct_order$taxo_order))

tryrow <- tryrow %>% select(-taxo_order,-taxo_family,-taxo_genus,-taxo_species)

predicted <- pruned_fit %>% predict(tryrow, "class") 
print(as.character(predicted))



## ----RF Model---------------------------------------------------------------------------------------------------------
#we will start with the tzNA table, on the order level
tz_train <- tzNA_order_parts$trainDS %>% select(-taxo_family,-taxo_genus,-taxo_species,-forearmlen_mm,-range_km2,-mid_lat,-mid_lng)
tz_test <- tzNA_order_parts$testDS %>% select(-taxo_family,-taxo_genus,-taxo_species,-forearmlen_mm,-range_km2,-mid_lat,-mid_lng)



## ----random forests, fig.width=12,fig.height=16-----------------------------------------------------------------------
set.seed(444, sample.kind = "Rounding")
##--------------------------------------
# Random Forest prediction of order data
library(randomForest)
rf_fit <- randomForest(
  as.factor(taxo_order)~., 
  data=tz_train,
  na.action=na.roughfix)

#print(rf_fit) # view results
#importance(rf_fit) # importance of each predictor 


## ----predict rf, fig.width=12,fig.height=16---------------------------------------------------------------------------
##--------------------------------------
# generate confusion matrix
prx <- predict(object=rf_fit,tz_test[-1],type="class")

tries <-length(as.factor(tz_test$taxo_order))
predictions <- sum(!is.na(prx))

tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)

cm <- confusionMatrix(data=prx,
                      reference=tz_actual, 
                      mode = "everything")

overall_accuracy <- cm$overall["Accuracy"]

measure_df4 <- measuremodel(cm,"4- Random Forest","With NA's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df4)
measure_df_all %>% knitr::kable()

#cm$byClass %>% knitr::kable()


## ----check results rf, fig.width=16,fig.height=16---------------------------------------------------------------------
getcmplot(cm,"Random Forest - With NA Values")


## ----summarize--------------------------------------------------------------------------------------------------------
#we will start with the tzNA table, on the order level
tz_train <- tzNA_order_parts$trainDS %>% select(-taxo_family,-taxo_genus,-taxo_species)
tz_test <- tzNA_order_parts$testDS %>% select(-taxo_family,-taxo_genus,-taxo_species)

tz_train[is.na(tz_train)]<-0
tz_test[is.na(tz_test)]<-0

##--------------------------------------
#tz_train %>% group_by(taxo_order) %>% summarize(group_count = n())
#tz_test %>% group_by(taxo_order) %>% summarize(group_count = n())


## ----impute 0 rfmodel, fig.width=16,fig.height=12---------------------------------------------------------------------
set.seed(555, sample.kind = "Rounding")
# Random Forest prediction of order data
library(randomForest)
rf_fit <- randomForest(
  as.factor(taxo_order)~., 
  data=tz_train,
  na.action=na.roughfix)


#print(rf_fit) # view results
#importance(rf_fit) # importance of each predictor 

##--------------------------------------

prx <- predict(object=rf_fit,tz_test[-1],type="class")

tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
cm <- confusionMatrix(data=prx,reference=tz_actual, mode = "everything")
overall_accuracy <- cm$overall["Accuracy"]

measure_df5 <- measuremodel(cm,"5- Random Forest","With 0's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df5)
measure_df_all %>% knitr::kable()


## ----impute 0 visual, fig.width=16,fig.height=12----------------------------------------------------------------------
##--------------------------------------
# F1 scores by class
dfcm <- data.frame(cm$byClass)
setDT(dfcm, keep.rownames = TRUE)
dfcm <- dfcm %>% mutate(note = ifelse(F1 == 1,"All Correct",ifelse(F1 == 0,"All Wrong",round(F1,2))))

#plot the F1 score by class
dfcm %>% ggplot() + 
  geom_text(aes(F1,rn,label=note),color="purple",size=4,nudge_y=.5) + 
  geom_point(aes(Precision,rn),color="red",size=4) + 
  geom_point(aes(Recall,rn),color="orange",size=4) + 
  geom_point(aes(F1,rn),color="purple",size=4) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="top")



## ----check results rf 2, fig.width=16,fig.height=16-------------------------------------------------------------------
getcmplot(cm,"Random Forest - With 0's Imputed")


## ---- echo=TRUE-------------------------------------------------------------------------------------------------------

oversample <- function(trainDS, min_group, max_draws, grouping){
  
  trainDS2 <- trainDS
 
  minority_ord <- trainDS %>% 
    group_by(!!sym(grouping)) %>% 
    summarize(group_count =  n()) %>% 
    filter(group_count <= min_group) 
  
    minorityrows <- trainDS %>% inner_join(minority_ord, by=grouping)
    minorityrows <- minorityrows %>% select(-group_count)
    minorityrows
  
    trainDS2 <- trainDS2 %>% bind_rows(minorityrows %>% sample_n(max_draws,replace=TRUE))
    
  trainDS2
}




## ----impute 0 and oversample------------------------------------------------------------------------------------------
set.seed(1, sample.kind = "Rounding")

#set the number of oversamples to 1.5-2x the original row count
samplerows <-5000

#we will start with the tzNA table, on the order level
tz_train <- tzNA_order_parts$trainDS %>% select(-taxo_family,-taxo_genus,-taxo_species)
tz_test <- tzNA_order_parts$testDS %>% select(-taxo_family,-taxo_genus,-taxo_species)

#impute 0's
tz_train[is.na(tz_train)]<-0
tz_test[is.na(tz_test)]<-0

#view group counts before oversampling
tz_train %>% group_by(taxo_order) %>% summarize(group_count =  n()) %>% slice(1:10) %>% knitr::kable()

#run oversampling routine
tz_train <- oversample(tz_train,median_group_count,samplerows,'taxo_order')

#view group counts after oversampling
tz_train %>%  group_by(taxo_order) %>% summarize(group_count =  n()) %>% slice(1:10) %>% knitr::kable()



## ----impute 0 oversampled, fig.width=16,fig.height=12-----------------------------------------------------------------
set.seed(666, sample.kind = "Rounding")
#summary(tz_train)
# Random Forest prediction of order data
library(randomForest)
rf_fit <- randomForest(
  as.factor(taxo_order)~., 
  data=tz_train,
  na.action=na.roughfix)


#print(rf_fit) # view results
#importance(rf_fit) # importance of each predictor 

##--------------------------------------

prx <- predict(object=rf_fit,tz_test[-1],type="class")

tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
cm <- confusionMatrix(data=prx,reference=tz_actual, mode = "everything")
overall_accuracy <- cm$overall["Accuracy"]

measure_df5 <- measuremodel(cm,"6- Random Forest","With 0's, Oversampled","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df5)
measure_df_all  %>% knitr::kable()

total_predicted <- measure_df5$Predicted


## ----check results os, fig.width=16,fig.height=16---------------------------------------------------------------------

cmplot <- getcmplot(cm,"Random Forest - Oversampled, With 0's Imputed")
cmplot + 
   
  annotate("text",x=15,y=20,color="blue",label="More minority classes being predicted")  + 
   
   annotate("path",color="blue",
   x=10+ r*cos(seq(0,2*pi,length.out=100)),
   y=20+ r*sin(seq(0,2*pi,length.out=100))) +
   annotate("path",color="blue",
   x=11+ r*cos(seq(0,2*pi,length.out=100)),
   y=19+ r*sin(seq(0,2*pi,length.out=100))) +
   annotate("path",color="blue",
   x=19+ r*cos(seq(0,2*pi,length.out=100)),
   y=11+ r*sin(seq(0,2*pi,length.out=100))) 


## ---- fig.width=12,fig.height=8---------------------------------------------------------------------------------------
#tzNA is the tinyzoo version that replaced the -999 with a standard R NA value

library(dplyr)

meanByGenus <- function(df, field) {
  rt <- data.frame()
  r2 <- data.frame()
  
  rt <- df %>%
    filter(!is.na(!!sym(field))) %>%
    group_by(taxo_order, taxo_family, taxo_genus) %>%
    summarize(imputedmean = mean(!!sym(field)))
  rt
  
  r2 <- df %>% left_join(rt, by=c("taxo_order"="taxo_order","taxo_family"="taxo_family","taxo_genus"="taxo_genus")) %>% 
    mutate(is_missing = ifelse(is.na(!!sym(field)),1,0)) %>% 
    mutate(replacement = ifelse(is_missing == 1,imputedmean,!!sym(field)))
  r2
 
}

readout <- data.frame()
loopGenus <- function(tzMEAN,field){
  
  before <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  tzMEAN <- meanByGenus(tzMEAN, field)
  tzMEAN[[field]] = tzMEAN[['replacement']]
  tzMEAN <- tzMEAN %>% select(-replacement,-is_missing,-imputedmean)
  after <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  diff <- before - after
  
  #print(paste(field, diff,"imputed - ",before,"before - ",after,"after"))
  tzMEAN

}

meanByFamily <- function(df, field) {
  rt <- data.frame()
  r2 <- data.frame()
  
  rt <- df %>%
    filter(!is.na(!!sym(field))) %>%
    group_by(taxo_order, taxo_family) %>%
    summarize(imputedmean = mean(!!sym(field)))
  rt
  
  r2 <- df %>% left_join(rt, by=c("taxo_order"="taxo_order","taxo_family"="taxo_family")) %>% 
    mutate(is_missing = ifelse(is.na(!!sym(field)),1,0)) %>% 
    mutate(replacement = ifelse(is_missing == 1,imputedmean,!!sym(field)))
  r2
 
}

readout <- data.frame()
loopFamily <- function(tzMEAN,field){
  
  before <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  tzMEAN <- meanByFamily(tzMEAN, field)
  tzMEAN[[field]] = tzMEAN[['replacement']]
  tzMEAN <- tzMEAN %>% select(-replacement,-is_missing,-imputedmean)
  after <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  diff <- before - after
  
  #print(paste(field, diff,"imputed - ",before,"before - ",after,"after"))
  tzMEAN

}

meanByOrder <- function(df, field) {
  rt <- data.frame()
  r2 <- data.frame()
  
  rt <- df %>%
    filter(!is.na(!!sym(field))) %>%
    group_by(taxo_order) %>%
    summarize(imputedmean = mean(!!sym(field)))
  rt
  
  r2 <- df %>% left_join(rt, by=c("taxo_order"="taxo_order")) %>% 
    mutate(is_missing = ifelse(is.na(!!sym(field)),1,0)) %>% 
    mutate(replacement = ifelse(is_missing == 1,imputedmean,!!sym(field)))
  r2
 
}

readout <- data.frame()
loopOrder <- function(tzMEAN,field){
  
  before <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  tzMEAN <- meanByOrder(tzMEAN, field)
  tzMEAN[[field]] = tzMEAN[['replacement']]
  tzMEAN <- tzMEAN %>% select(-replacement,-is_missing,-imputedmean)
  after <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  diff <- before - after
  
  #print(paste(field, diff,"imputed - ",before,"before - ",after,"after"))
  tzMEAN

}



meanByClass <- function(df, field) {
  rt <- data.frame()
  r2 <- data.frame()
  
  rt <- df %>%
    filter(!is.na(!!sym(field))) %>%
    group_by(taxo_class) %>%
    summarize(imputedmean = mean(!!sym(field)))
  rt
  
  r2 <- df %>% left_join(rt, by=c("taxo_class"="taxo_class")) %>% 
    mutate(is_missing = ifelse(is.na(!!sym(field)),1,0)) %>% 
    mutate(replacement = ifelse(is_missing == 1,imputedmean,!!sym(field)))
  r2
 
}

readout <- data.frame()
loopClass <- function(tzMEAN,field){
  
  before <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  tzMEAN <- meanByClass(tzMEAN, field)
  tzMEAN[[field]] = tzMEAN[['replacement']]
  tzMEAN <- tzMEAN %>% select(-replacement,-is_missing,-imputedmean)
  after <- sum(is.na(tzMEAN %>% select(!!sym(field))))
  diff <- before - after
  
  #print(paste(field, diff,"imputed - ",before,"before - ",after,"after"))
  tzMEAN

}






imputeMeans <- function(tzMEAN){
  
  tzMEAN <- loopGenus(tzMEAN,'headlen_mm')
  tzMEAN <- loopGenus(tzMEAN,'mass_grams')
  tzMEAN <- loopGenus(tzMEAN,'litter_size')
  tzMEAN <- loopGenus(tzMEAN,'litters_peryear')
  tzMEAN <- loopGenus(tzMEAN,'gestation_days')
  tzMEAN <- loopGenus(tzMEAN,'longevity_months')
  tzMEAN <- loopGenus(tzMEAN,'diet_breadth')
  tzMEAN <- loopGenus(tzMEAN,'pop_density')
  tzMEAN <- loopGenus(tzMEAN,'trophic_level')
  tzMEAN <- loopGenus(tzMEAN,'terrestriality')
  tzMEAN <- loopGenus(tzMEAN,'sex_maturity')
  tzMEAN <- loopGenus(tzMEAN,'wean_age')
  tzMEAN <- loopGenus(tzMEAN,'activity_cycle')
  tzMEAN <- loopGenus(tzMEAN,'forearmlen_mm')
  tzMEAN <- loopGenus(tzMEAN,'mid_lat')
  tzMEAN <- loopGenus(tzMEAN,'mid_lng')
  tzMEAN <- loopGenus(tzMEAN,'range_km2')
  
  tzMEAN <- loopFamily(tzMEAN,'headlen_mm')
  tzMEAN <- loopFamily(tzMEAN,'mass_grams')
  tzMEAN <- loopFamily(tzMEAN,'litter_size')
  tzMEAN <- loopFamily(tzMEAN,'litters_peryear')
  tzMEAN <- loopFamily(tzMEAN,'gestation_days')
  tzMEAN <- loopFamily(tzMEAN,'longevity_months')
  tzMEAN <- loopFamily(tzMEAN,'diet_breadth')
  tzMEAN <- loopFamily(tzMEAN,'pop_density')
  tzMEAN <- loopFamily(tzMEAN,'trophic_level')
  tzMEAN <- loopFamily(tzMEAN,'terrestriality')
  tzMEAN <- loopFamily(tzMEAN,'sex_maturity')
  tzMEAN <- loopFamily(tzMEAN,'wean_age')
  tzMEAN <- loopFamily(tzMEAN,'activity_cycle')
  tzMEAN <- loopFamily(tzMEAN,'forearmlen_mm')
  tzMEAN <- loopFamily(tzMEAN,'mid_lat')
  tzMEAN <- loopFamily(tzMEAN,'mid_lng')
  tzMEAN <- loopFamily(tzMEAN,'range_km2')
  
  tzMEAN <- loopOrder(tzMEAN,'headlen_mm')
  tzMEAN <- loopOrder(tzMEAN,'mass_grams')
  tzMEAN <- loopOrder(tzMEAN,'litter_size')
  tzMEAN <- loopOrder(tzMEAN,'litters_peryear')
  tzMEAN <- loopOrder(tzMEAN,'gestation_days')
  tzMEAN <- loopOrder(tzMEAN,'longevity_months')
  tzMEAN <- loopOrder(tzMEAN,'diet_breadth')
  tzMEAN <- loopOrder(tzMEAN,'pop_density')
  tzMEAN <- loopOrder(tzMEAN,'trophic_level')
  tzMEAN <- loopOrder(tzMEAN,'terrestriality')
  tzMEAN <- loopOrder(tzMEAN,'sex_maturity')
  tzMEAN <- loopOrder(tzMEAN,'wean_age')
  tzMEAN <- loopOrder(tzMEAN,'activity_cycle')
  tzMEAN <- loopOrder(tzMEAN,'forearmlen_mm')
  tzMEAN <- loopOrder(tzMEAN,'mid_lat')
  tzMEAN <- loopOrder(tzMEAN,'mid_lng')
  tzMEAN <- loopOrder(tzMEAN,'range_km2')

  tzMEAN <- tzMEAN %>% mutate(taxo_class = "Mammalia")
  tzMEAN <- loopClass(tzMEAN,'headlen_mm')
  tzMEAN <- loopClass(tzMEAN,'mass_grams')
  tzMEAN <- loopClass(tzMEAN,'litter_size')
  tzMEAN <- loopClass(tzMEAN,'litters_peryear')
  tzMEAN <- loopClass(tzMEAN,'gestation_days')
  tzMEAN <- loopClass(tzMEAN,'longevity_months')
  tzMEAN <- loopClass(tzMEAN,'diet_breadth')
  tzMEAN <- loopClass(tzMEAN,'pop_density')
  tzMEAN <- loopClass(tzMEAN,'trophic_level')
  tzMEAN <- loopClass(tzMEAN,'terrestriality')
  tzMEAN <- loopClass(tzMEAN,'sex_maturity')
  tzMEAN <- loopClass(tzMEAN,'wean_age')
  tzMEAN <- loopClass(tzMEAN,'activity_cycle')
  tzMEAN <- loopClass(tzMEAN,'forearmlen_mm')
  tzMEAN <- loopClass(tzMEAN,'mid_lat')
  tzMEAN <- loopClass(tzMEAN,'mid_lng')
  tzMEAN <- loopClass(tzMEAN,'range_km2')
  tzMEAN <- tzMEAN %>% select(-taxo_class)
  
  #remove estimated forewarm length for non flying mammals
#remove geo data for marine mammals
#tzMEAN <- tzMEAN %>% mutate(forearmlen_mm = ifelse(taxo_order == 'Chiroptera',forearmlen_mm,0))

#tzMEAN <- tzMEAN %>% mutate(mid_lat = ifelse(taxo_order == 'Cetacea' | taxo_order == 'Sirenia',0,mid_lat))
#tzMEAN <- tzMEAN %>% mutate(mid_lng = ifelse(taxo_order == 'Cetacea' | taxo_order == 'Sirenia',0,mid_lng))
#tzMEAN <- tzMEAN %>% mutate(range_km2 = ifelse(taxo_order == 'Cetacea' | taxo_order == 'Sirenia',0,range_km2))
}

tzMEAN <- tzNA_order_parts$trainDS
tzMEAN <- imputeMeans(tzMEAN)

tzMEAN %>% slice(1:10) %>% select(taxo_order,taxo_family,taxo_genus,taxo_species,mass_grams,headlen_mm) %>% knitr::kable()


## ---------------------------------------------------------------------------------------------------------------------
#clear some of the varianbvles that are not "universally" recorded
#we will address ways to use these in the construction of an available case moodel
tz_train <- tzMEAN %>% select(-taxo_family,-taxo_genus,-taxo_species,-forearmlen_mm,-mid_lat,-mid_lng,-range_km2)
tz_test <- tzNA_order_parts$testDS %>% select(-forearmlen_mm,-mid_lat,-mid_lng,-range_km2)



## ---------------------------------------------------------------------------------------------------------------------
set.seed(777, sample.kind = "Rounding")


# Random Forest prediction of order data
library(randomForest)
rf_fit <- randomForest(
  as.factor(taxo_order)~., 
  data=tz_train,
  na.action=na.roughfix)

##--------------------------------------
prx <- predict(object=rf_fit,tz_test,type="class")
tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
cm <- confusionMatrix(data=prx,reference=tz_actual)
overall_accuracy <- cm$overall["Accuracy"]

##--------------------------------------

measure_df9 <- measuremodel(cm,"7- Random Forest","MEAN Imputation with NA's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df9)
measure_df_all %>% knitr::kable()



## ----check results mean, fig.width=16,fig.height=16-------------------------------------------------------------------
getcmplot(cm,"Random Forest - MEAN Imputation - Complete Case Analysis")


## ---------------------------------------------------------------------------------------------------------------------
#clear some of the varianbvles that are not "universally" recorded
#we will address ways to use these in the construction of an available case moodel

tz_train <- tzMEAN %>% select(-taxo_family,-taxo_genus,-taxo_species,-forearmlen_mm,-mid_lat,-mid_lng,-range_km2)
tz_test <- tzNA_order_parts$testDS %>% select(-forearmlen_mm,-mid_lat,-mid_lng,-range_km2)

tz_test[is.na(tz_test)] <- 0


## ---------------------------------------------------------------------------------------------------------------------
set.seed(888, sample.kind = "Rounding")

# Random Forest prediction of order data
library(randomForest)
rf_fit <- randomForest(
  as.factor(taxo_order)~., 
  data=tz_train,
  na.action=na.roughfix)

##--------------------------------------
prx <- predict(object=rf_fit,tz_test,type="class")
tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
cm <- confusionMatrix(data=prx,reference=tz_actual)
overall_accuracy <- cm$overall["Accuracy"]

##--------------------------------------

measure_df9 <- measuremodel(cm,"8- Random Forest","MEAN Imputation with 0's","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df9)
measure_df_all %>% knitr::kable()



## ----check results mean 2, fig.width=16,fig.height=16-----------------------------------------------------------------
getcmplot(cm,"Random Forest - MEAN Imputation with 0's")


## ----basic model, echo=TRUE-------------------------------------------------------------------------------------------
#SIMPLE NON-RECURSIVE MODEL TO PREDICT ORDER
predictorder <- function(trainDS,testDS,impute,oversamplemin,oversamplemax){
  set.seed(1, sample.kind = "Rounding")

  #IMPUTE 0 IF INDICATED
  if(impute == 'zero'){
     trainDS[is.na(trainDS)]<-0
     testDS[is.na(testDS)]<-0
  }
  
  #oversample if indicated
  if(oversamplemin > 0){
    trainDS <- oversample(trainDS,oversamplemin,oversamplemax,'taxo_order')
  }
  
  trainDS <- trainDS %>% select(-taxo_family,-taxo_genus,-taxo_species)
  testDS <- testDS %>% select(-taxo_family,-taxo_genus,-taxo_species)
  
  #set.seed(1, sample.kind = "Rounding")
  # Random Forest prediction of order data
  library(randomForest)
  rf_fit <- randomForest(
    as.factor(taxo_order)~., 
    data=trainDS,
    na.action=na.roughfix
    )
  
  ##--------------------------------------
  prx <- predict(object=rf_fit,testDS,type="class")
  tz_actual <- tz_test %>% mutate(taxo_order = as.factor(taxo_order)) %>% pull(taxo_order)
  
  cm <- confusionMatrix(data=prx,reference=tz_actual)
  
  returnobj <-list(
                  confusionmatrix=cm,
                  predictions=prx
                  )

  class(returnobj) <- "rfresults"
  returnobj
}


## ----predictgenus available case model, warning=FALSE, message=FALSE, echo=TRUE---------------------------------------

extract_columns <- function(data, desired_columns) {
    extracted_data <- data %>%
        select_(.dots = desired_columns)
    return(extracted_data)
}

predictgenusac <- function(trainDS,testRow,last_a){
  
  predicted_order <- ''
  predicted_family <- ''
  predicted_genus <- ''
  
  ##--------------------------------------
  #SETUP - LEAVE ONLY ORDER DATA
  train_order <- trainDS %>% select(-taxo_family, -taxo_genus, -taxo_species)
  test_order <- testRow %>% select(-taxo_family, -taxo_genus, -taxo_species)
  
  #remove the reference class for the purpose of making a prediction
  test_order_pred <- test_order %>% select(-taxo_order)
  onerow <- testRow
  
  #Select a list of names of the columns that are in the test set
  usecolumns <- colnames(test_order_pred)[colSums(!is.na(test_order_pred)) > 0]
  test_columns <- length(usecolumns)
 
  #print(paste("Test Columns:",test_columns))
  #print(paste("Row:",last_a))
  
  #Extract those columns only from the training set 
  train_extracted <- extract_columns(train_order,usecolumns) 
  test_extracted <- extract_columns(onerow,usecolumns)
 
  #Reconstruct the training table with class and predictors only
  train_order <- train_order %>% select(taxo_order) %>% bind_cols(train_extracted) 
  train_order <- train_order %>% drop_na()
  train_order$taxo_order <- droplevels(train_order$taxo_order)
  
  
  
  #-----------------------------------------
  #PREDICT ORDER
  library(randomForest)
  rf_fit_order <- randomForest(
    as.factor(taxo_order)~., 
    data=train_order,
    na.action=na.roughfix
    )

  prx <- predict(object=rf_fit_order, 
                 test_order_pred,
                 type="class")
  
  predicted_order <- prx
  predicted_order <- as.character(predicted_order)
   #-----------------------------------------
    #CHECK FOR SUB FAMILES
   
  
  # print(paste("Predicted Order:",predicted_order))
  
   sub_family <- trainDS %>% 
    filter(as.character(taxo_order) == as.character(predicted_order)) %>% 
   group_by(taxo_family) %>% summarize(groupcount = n())
  
   num_fams <- count(sub_family)$n
 
   #print(paste("num_fams:",num_fams))
  
   if(num_fams == 1){
     predicted_family <- sub_family %>% pull(taxo_family)
     predicted_family <- as.character(predicted_family)
     
     
     
      }else{
     
        
   ##--------------------------------------
    #SETUP FAMILY DATA
      train_family <- trainDS %>% 
      filter(as.character(taxo_order) == as.character(predicted_order)) %>% 
      select(-taxo_order, -taxo_genus, -taxo_species) %>% 
      mutate(taxo_family = as.character(taxo_family))  %>% 
      mutate(taxo_family = as.factor(taxo_family)) 
    
      #Leave only Family Class
      test_family <- testRow %>% select(-taxo_order, -taxo_genus, -taxo_species) 
      
      #Leave only predictors
      test_family_pred <- test_family %>% select(-taxo_family)
   
      #AVAILABLE CASE ANALYSIS
      #Select the columsn that in the test set
      usecolumns <- colnames(test_family_pred)[colSums(!is.na(test_family_pred)) > 0]
     
      #Extract those columns only from the training set 
      train_extracted <- extract_columns(train_family,usecolumns) 
      test_extracted <- extract_columns(onerow,usecolumns)
     
      #Reconstruct the training table with class and predictors only
      train_family <- train_family %>% select(taxo_family) %>% bind_cols(train_extracted) 
      train_family <- train_family %>% drop_na()
      train_family$taxo_family <- droplevels(train_family$taxo_family)
      
      if (nrow(train_family)>1 ){
          #-----------------------------------------
          #PREDICT FAMILY
          rf_fit_family <- randomForest(
          as.factor(taxo_family)~., 
          data=train_family,
          na.action=na.roughfix
          )
        
          prx <- predict(object=rf_fit_family,test_family_pred,type="class")
        
          predicted_family <- prx
          predicted_family <- as.character(predicted_family)
      
      }else{
        
         predicted_family <- train_family %>% pull(taxo_family)
        predicted_family <- as.character(predicted_family)
        }
      }
   
   #print(paste("Predicted Family:",predicted_family))
  
      #-----------------------------------------
      #CHECK FOR SUB GENUS
     
      sub_genus <- trainDS %>% 
      filter(as.character(taxo_order) == as.character(predicted_order) & as.character(taxo_family) == as.character(predicted_family)) %>% 
      group_by(taxo_genus) %>% summarize(groupcount = n())
      num_genus <- count(sub_genus)$n

      #print(paste("num_genus:",num_genus))
  
      if(num_genus == 1){
         predicted_genus <- sub_genus %>% pull(taxo_genus)
         predicted_genus <- as.character(predicted_genus)
        
      }else{
     
          ##--------------------------------------
          #SETUP GENUS DATA
         
         #leave genus & Predictors only in training data
          train_genus <- trainDS %>% 
          filter(as.character(taxo_order) == as.character(predicted_order) & as.character(taxo_family) == as.character(predicted_family)) %>% 
          select(-taxo_order, -taxo_family, -taxo_species)  %>%
          mutate(taxo_genus = as.character(taxo_genus))  %>% 
          mutate(taxo_genus = as.factor(taxo_genus)) 
        
          #leave predictors and genus class only
          test_genus <- testRow %>% 
          select(-taxo_order, -taxo_family, -taxo_species)
          
          #leave predictorsonly
          test_genus_pred <- test_genus %>% select(-taxo_genus)
       
          #AVAILABLE CASE ANALYSIS
          #Select the columsn that in the test set
          usecolumns <- colnames(test_genus_pred)[colSums(!is.na(test_genus_pred)) > 0]
          
          #Extract those columns only from the training set 
          train_extracted <- extract_columns(train_genus,usecolumns) 
          test_extracted <- extract_columns(onerow,usecolumns)
         
          #Reconstruct the training table with class and predictors only
          train_genus <- train_genus %>% select(taxo_genus) %>% bind_cols(train_extracted) 
          train_genus <- train_genus %>% drop_na()
          train_genus$taxo_genus <- droplevels(train_genus$taxo_genus)
          
          
          if (nrow(train_genus)>1 ){
              #-----------------------------------------
            #PREDICT GENUS
            rf_fit_genus <- randomForest(
            as.factor(taxo_genus)~., 
            data=train_genus,
            na.action=na.roughfix
            )
          
            prx <- predict(object=rf_fit_genus,test_genus_pred,type="class")
          
            predicted_genus <- prx
            predicted_genus <- as.character(predicted_genus)
            predicted_genus
         }else{
            predicted_genus <- train_genus %>% pull(taxo_genus)
            predicted_genus <- as.character(predicted_genus)
         }
          
      
    
   }
 
  #print(paste("Predicted Genus:",predicted_genus))
  
  
  returnobj <- list(
    order=predicted_order,
    family=predicted_family,
    genus=predicted_genus,
    test_cols=test_columns
  )
 
  returnobj
  
 
} 


## ----acgenusmodel container, warning=FALSE, message=FALSE, echo=TRUE--------------------------------------------------

acgenusmodel <- function(trainDS,testDS,impute,oversamplemin,oversamplemax){
#create a table of the results of bernoulli trails using the hold-out data to train the recursive model
#the recursive model must run one row at a time since each row it chooses spawns a new model training process
trials <- data.frame()
attempts <- seq(1:nrow(testDS))
set.seed(1, sample.kind = "Rounding")

#impute zero if indicated
if(impute == 'zero'){
     trainDS[is.na(trainDS)]<-0
     testDS[is.na(testDS)]<-0
}
#impute means if indicated
if(impute == 'mean'){
    trainDS <- imputeMeans(trainDS)
}

#set the training to the entire dataset
trainDS_recursive <- trainDS

if(oversamplemin > 0){
    trainDS_recursive_os <- oversample(trainDS_recursive,oversamplemin,oversamplemax,'taxo_order')
}else{
    trainDS_recursive_os <- trainDS_recursive
}

#make some predictions
for(a in attempts){
  
  last_a <- a
  onerow <- testDS %>% slice(a:a)
  predicted <- predictgenusac(trainDS_recursive_os,onerow,last_a)
 
  order_match <- (as.character(predicted$order) == as.character(onerow$taxo_order))
  family_match <- (as.character(predicted$family) == as.character(onerow$taxo_family))
  genus_match <- (as.character(predicted$genus) == as.character(onerow$taxo_genus))
  
  newrow <- data.frame(pred_order=as.character(predicted$order),
                       pred_fam=as.character(predicted$family),
                       pred_genus=as.character(predicted$genus),
                       correct_order=as.character(onerow$taxo_order),
                       correct_fam=as.character(onerow$taxo_family),
                       correct_genus=as.character(onerow$taxo_genus),
                       correct_species=as.character(onerow$taxo_species),
                       test_cols=as.character(predicted$test_cols),
                       order_correct=order_match,
                       family_correct=family_match,
                       genus_correct=genus_match
                       )
  
  trials <- trials %>% bind_rows(newrow)
  
}

order_acc2 <- mean(trials$order_correct)
family_acc2 <- mean(trials$family_correct)
genus_acc2 <- mean(trials$genus_correct)

trials <- trials %>% left_join(order_info, by = c("pred_order" = "taxo_order"))
trials <- trials %>% left_join(order_info, by = c("correct_order" = "taxo_order"))

trials
}



## ----get basic model cm-----------------------------------------------------------------------------------------------
tz_train <- tzNA_species_holdout$trainDS 
tz_test <- tzNA_species_holdout$testDS

#tz_train %>% group_by(taxo_order) %>% summarize(groupcount = n())
#tz_test %>% group_by(taxo_order) %>% summarize(groupcount = n())

prx <- predictorder(tz_train,tz_test,'zero',median_group_count,5000)
cm <- prx$confusionmatrix
overall_accuracy <- cm$overall["Accuracy"]



## ----check results basic model, fig.width=16,fig.height=16------------------------------------------------------------
getcmplot(cm,"Random Forest - Basic Model - Final Test - Rank: Order")


## ----measurebasicfinal------------------------------------------------------------------------------------------------
measure_df10 <- measuremodel(cm,"9- Random Forest","Basic Model - Order - FINAL","Order")
measure_df_all <- measure_df_all %>% bind_rows(measure_df10)
measure_df_all %>% knitr::kable()


## ----run final acmodel, warning=FALSE, message=FALSE------------------------------------------------------------------
maxrows <- 5000

#set teh data to use
trainDS <- tzNA_species_holdout$trainDS
testDS <- tzNA_species_holdout$testDS
#run the model with mean imputation, oversampling orders 
#with below the median (20) rows, up to double the size of the original trainingdata (4138 rows)
trials <- acgenusmodel(trainDS,testDS,'mean',median_group_count,maxrows)


## ----peek results-----------------------------------------------------------------------------------------------------
trials %>% sample_n(10) %>% select(pred_order,pred_fam,pred_genus,correct_order,correct_fam,correct_genus) %>% knitr::kable()


## ----get final order cm-----------------------------------------------------------------------------------------------
correct_ords <- trials  %>% pull(correct_order)
pred_ords <- trials %>% pull(pred_order)
check_ords <- c(correct_ords,pred_ords)  

ord_levels <- levels(droplevels(as.factor(check_ords)))
cm <- confusionMatrix(data=factor(trials$pred_order,levels =ord_levels),reference=factor(trials$correct_order,levels = ord_levels))



## ----check results final order, fig.width=16,fig.height=16------------------------------------------------------------
getcmplot(cm,"Random Forest - Available Case Model - Final Test - Rank: Order","All Predictions for Holdout data")


## ----get final family cm----------------------------------------------------------------------------------------------
correct_fams <- trials %>% filter(correct_order == 'Rodentia')  %>% pull(correct_fam)
pred_fams <- trials %>% filter(pred_order == 'Rodentia')  %>% pull(pred_fam)
check_fams <- c(correct_fams,pred_fams)  

fam_levels <- levels(droplevels(as.factor(check_fams)))
cm <- confusionMatrix(data=factor(trials$pred_fam,levels =fam_levels),reference=factor(trials$correct_fam,levels = fam_levels))



## ----check results final fam, fig.width=16,fig.height=16--------------------------------------------------------------
getcmplot(cm,"Random Forest - Available Case Model - Final Test - Rank: Family","Predictions of Families in Order: Rodentia for Holdout data")


## ----get final genus cm-----------------------------------------------------------------------------------------------
correct_gens <- trials %>% filter(correct_order == 'Rodentia') %>% filter(correct_fam == 'Cricetidae') %>% pull(correct_genus)
pred_gens <- trials %>% filter(pred_order == 'Rodentia') %>% filter(pred_fam == 'Cricetidae') %>% pull(pred_genus)
check_gens <- c(correct_gens,pred_gens)  

genus_levels <- levels(droplevels(as.factor(check_gens)))
cm <- confusionMatrix(data=factor(trials$pred_genus,levels =genus_levels),reference=factor(trials$correct_genus,levels = genus_levels))


## ----check results final genus, fig.width=16,fig.height=16------------------------------------------------------------
getcmplot(cm,"Random Forest - Available Case Model - Final Test - Rank: Genus","Predictions of Genus in Order: Rodentia > Family: Cricetidae for Holdout data")


## ----measure final order----------------------------------------------------------------------------------------------
correct_ords <- trials  %>% pull(correct_order)
pred_ords <- trials %>% pull(pred_order)
check_ords <- c(correct_ords,pred_ords)  

ord_levels <- levels(droplevels(as.factor(check_ords)))
cm <- confusionMatrix(data=factor(trials$pred_order,levels =ord_levels),reference=factor(trials$correct_order,levels = ord_levels))



## ----measureadvfinal order--------------------------------------------------------------------------------------------
orders_predicted <- nrow(trials %>% group_by(pred_order) %>% 
                           summarize(ordercount = n()))

measure_df11 <- measuremodel(cm,"10- Random Forest","Advanced Model - Order - FINAL","Order") %>% mutate(Predicted = orders_predicted)
measure_df_all <- measure_df_all %>% bind_rows(measure_df11)


## ----measure final family---------------------------------------------------------------------------------------------
correct_fams <- trials  %>% pull(correct_fam)
pred_fams <- trials %>% pull(pred_fam)
check_fams <- c(correct_fams,pred_fams)  

fam_levels <- levels(droplevels(as.factor(check_fams)))
cm <- confusionMatrix(data=factor(trials$pred_fam,levels =fam_levels),reference=factor(trials$correct_fam,levels = fam_levels))



## ----measureadvfinal family-------------------------------------------------------------------------------------------
fams_predicted <- nrow(trials %>% group_by(pred_order,pred_fam) %>% 
                           summarize(ordercount = n()))

measure_df12 <- measuremodel(cm,"11- Random Forest","Advanced Model - Family - FINAL","Family") %>% mutate(Predicted = fams_predicted)
measure_df_all <- measure_df_all %>% bind_rows(measure_df12)


## ----measure final genus----------------------------------------------------------------------------------------------
correct_gens <- trials %>% pull(correct_genus)
pred_gens <- trials %>% filter(pred_genus != '') %>% pull(pred_genus)
check_gens <- c(correct_gens,pred_gens)  

genus_levels <- levels(droplevels(as.factor(check_gens)))
cm <- confusionMatrix(data=factor(trials$pred_genus,levels =genus_levels),reference=factor(trials$correct_genus,levels = genus_levels))


## ----measureadvfinal genus--------------------------------------------------------------------------------------------
genus_predicted <- nrow(trials %>% group_by(pred_order,pred_fam,pred_genus) %>% 
                           summarize(ordercount = n()))

measure_df13 <- measuremodel(cm,"12- Random Forest","Advanced Model - Genus -  FINAL","Genus") %>% mutate(Predicted = genus_predicted)
measure_df_all <- measure_df_all %>% bind_rows(measure_df13)
measure_df_all %>% knitr::kable()


## ----check accuracy by rank-------------------------------------------------------------------------------------------
orders_intraining <- nrow(tzNA_species_holdout$trainDS %>% 
                        group_by(taxo_order) %>% 
                        summarize(ordercount = n()))

orders_intest <- nrow(tzNA_species_holdout$testDS %>% 
                        group_by(taxo_order) %>% 
                        summarize(ordercount = n()))

fams_intraining <- nrow(tzNA_species_holdout$trainDS %>% 
                        group_by(taxo_order,taxo_family) %>% 
                        summarize(ordercount = n()))

fams_intest <- nrow(tzNA_species_holdout$testDS %>% 
                        group_by(taxo_order,taxo_family) %>% 
                        summarize(ordercount = n()))

genus_intraining <- nrow(tzNA_species_holdout$trainDS %>% 
                        group_by(taxo_order,taxo_family,taxo_genus) %>% 
                        summarize(ordercount = n()))

genus_intest <- nrow(tzNA_species_holdout$testDS %>% 
                        group_by(taxo_order,taxo_family,taxo_genus) %>% 
                        summarize(ordercount = n()))



order_acc <- mean(trials$order_correct)
family_acc <- mean(trials$family_correct)
genus_acc <- mean(trials$genus_correct)

#orders_intraining
#orders_predicted
#orders_intest

##fams_intraining
#fams_predicted
#fams_intest

#genus_intraining
#genus_predicted
#genus_intest

#order_acc
#family_acc
#genus_acc

