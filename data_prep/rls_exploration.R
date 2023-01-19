# RLS DATA
## exploration

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read RLS dataset
rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

# check variable names of the dataset
names(rls_2021_2022)

##### explore the location

unique(rls_2021_2022$country) 
## output: 
# "Australia"
unique(rls_2021_2022$area)
## output:
# [1] "Queensland"         "Tasmania"           "South Australia"    "New South Wales"    "Western Australia" 
# [6] "Other Territories"  "Victoria"           "Northern Territory"
unique(rls_2021_2022$ecoregion)
## output:
# [1] "Central and Southern Great Barrier Reef"   "Bassian"                                  
# [3] "South Australian Gulfs"                    "Manning-Hawkesbury"                       
# [5] "Houtman"                                   "Lord Howe and Norfolk Islands"            
# [7] "Leeuwin"                                   "Cape Howe"                                
# [9] "Shark Bay"                                 "Exmouth to Broome"                        
# [11] "Tweed-Moreton"                             "Ningaloo"                                 
# [13] "Coral Sea"                                 "Torres Strait Northern Great Barrier Reef"
# [15] "Gulf of Papua"                             "Arnhem Coast to Gulf of Carpenteria"      
# [17] "Bonaparte Coast"
unique(rls_2021_2022$realm)
## output:
# [1] "Central Indo-Pacific"  "Temperate Australasia"
unique(rls_2021_2022$location)
## output:
# [1] "QLD inshore - South"       "Tasmania - South East"     "Encounter"                 "D'Entrecasteaux & Derwent"
# [5] "Tinderbox"                 "Rocky Cape"                "Sydney"                    "Port Stephens"            
# [9] "Marmion"                   "Adelaide"                  "Norfolk Island"            "Shoalwater"               
# [13] "Maria Island"              "Ninepin Point"             "Jervis Bay"                "Flinders Island"          
# [17] "Abrolhos (WA)"             "Cape Howe"                 "Bicheno"                   "Rottnest Island"          
# [21] "QLD inshore - Central"     "GBR - Central"             "Shark Bay"                 "Pilbara"                  
# [25] "Solitary Islands"          "Byron"                     "Moreton"                   "Great Sandy"              
# [29] "Ningaloo Reef"             "GBR - South"               "Ningaloo Reef - North"     "NSW - Central North"      
# [33] "Coral Sea - Central"       "Exmouth"                   "Coral Sea - Far North"     "Torres Strait"            
# [37] "Wessel"                    "Arnhem"                    "Arafura"                   "Darwin"                   
# [41] "Oceanic Shoals"            "Ashmore/Cartier"           "Kimberley"                 "Carpentaria"              
# [45] "West Cape York"            "GBR - North"               "Tweed Heads"               "Yorke Peninsula"          
# [49] "Port Phillip Bay"          "Port Phillip Heads"        "Gold Coast"                "Lord Howe Island"         
# [53] "Geographe Bay"             "Tasmania - South"          "Port Stephens - North"

##### conclusion: the realm might be interesting in terms of species composition and distribution

##############################################

##### check survey identifier

length(unique(rls_2021_2022[["FID"]]))
## output:
# [1] 91410, this column contains an identifier for each survey
length(unique(rls_2021_2022[["survey_id"]]))
## output:
# [1] 1276
length(unique(rls_2021_2022[["site_code"]]))
## output:
# [1] 475
length(unique(rls_2021_2022[["site_name"]]))
## output:
# [1] 475
head(rls_2021_2022$geom)
length(unique(rls_2021_2022[["geom"]]))
## probably the specific coordinates of each observation
rls_2021_2022 %>% select(latitude, longitude, survey_date) %>% n_distinct()
## output:
# [1] 490

##### conclusion: FID and survey_id can be dropped
##### the other ids might be important if we take depths and blocks into account...
##### if we merge the data from different blocks and depths, we have 490 sites

##############################################

##### check coordinates

# geographical range
range(rls_2021_2022$latitude, na.rm = TRUE)
## output:
# [1] -43.53  -9.88
range(rls_2021_2022$survey_latitude, na.rm = TRUE)
## output:
# [1] -43.53  -9.88
range(rls_2021_2022$longitude, na.rm = TRUE)
## output:
# [1] 113.17 167.99
range(rls_2021_2022$survey_longitude, na.rm = TRUE)
## output:
# [1] 113.17 167.96

# let's look at the first rows of survey_latitude and survey_longitude
head(rls_2021_2022["survey_latitude"])
head(rls_2021_2022["survey_longitude"])
## it seems that these columns have some NAs

# how many rows are in those columns without NAs?
nrow(rls_2021_2022) - nrow(rls_2021_2022[complete.cases(rls_2021_2022$survey_latitude),]) # output: [1] 22014
nrow(rls_2021_2022) - nrow(rls_2021_2022[complete.cases(rls_2021_2022$survey_longitude),]) # output: [1] 22014
# only 22014 of 91410 rows do not have NAs

##### conclusion: rather use latitude and longitude instead of survey_latitude and survey_longitude

##############################################

##### specific survey data

# time period and survey time
range(rls_2021_2022$survey_date, na.rm = TRUE)
## output:
# [1] "2021-01-03" "2022-09-15"
s_time = rls_2021_2022 %>% select(hour) %>% distinct()
## surveys were done between 12pm (midnight) and 17pm

# program and method
unique(rls_2021_2022$program)
## output:
# [1] "RLS"  "ATRC", what is ATRC?
unique(rls_2021_2022$method)
## output:
# [1] 1,  makes sense in view of the RLS manual (method 1 -> fish)
unique(rls_2021_2022$block)
## output:
# [1] 2 1, check RLS manual, blocks can be merged

# physicial parameters
range(rls_2021_2022$visibility, na.rm = TRUE)
## output:
# [1]  2 50
range(rls_2021_2022$depth, na.rm = TRUE)
## output:
# [1]  1 22, check later if depth has an effect on the community composition

##### conclusion: how does the survey time affect the community data?
##### data of different blocks can be merged
##### check, if depth is a predictor of our community data

##############################################

##### community data

# taxonomy
unique(rls_2021_2022$phylum)
## output:
# [1] "Chordata" "Mollusca"
unique(rls_2021_2022$class)
## output:
# [1] "Actinopterygii" "Elasmobranchii" "Cephalopoda"    "Reptilia"       "Mammalia"
unique(rls_2021_2022$order)
## output:
# [1] "Perciformes"               "Eupercaria incertae sedis" "Orectolobiformes"          "Aulopiformes"             
# [5] "Beryciformes"              "Tetraodontiformes"         "Syngnathiformes"           "Gadiformes"               
# [9] "Anguilliformes"            "Carcharhiniformes"         "Scorpaeniformes"           "Teuthoidea"               
# [13] "Beloniformes"              "Torpediniformes"           "Rajiformes"                "Sepioidea"                
# [17] "Testudines"                "Myliobatiformes"           "Octopoda"                  "Siluriformes"             
# [21] "Heterodontiformes"         "Carnivora"                 "Clupeiformes"              "Zeiformes"                
# [25] "Pleuronectiformes"         "Batrachoidiformes"         "Gobiesociformes"           "Lamniformes"              
# [29] "Myopsida"                  "Squamata"                  NA                          "Acanthuriformes"          
# [33] "Callionymiformes"          "Blenniiformes"             "Kurtiformes"               "Atheriniformes"
unique(rls_2021_2022$family)
## output:
# [1] "Serranidae"         "Scaridae"           "Pseudochromidae"    "Pomacentridae"      "Lutjanidae"        
# [6] "Labridae"           "Gobiidae"           "Blenniidae"         "Apogonidae"         "Siganidae"         
# [11] "Nemipteridae"       "Orectolobidae"      "Ptereleotridae"     "Pomacanthidae"      "Chaetodontidae"    
# [16] "Microdesmidae"      "Pinguipedidae"      "Ephippidae"         "Acanthuridae"       "Tripterygiidae"    
# [21] "Plesiopidae"        "Synodontidae"       "Haemulidae"         "Caesionidae"        "Latidae"           
# [26] "Holocentridae"      "Lethrinidae"        "Pempheridae"        "Ostraciidae"        "Dinolestidae"      
# [31] "Carangidae"         "Monacanthidae"      "Cheilodactylidae"   "Kyphosidae"         "Odacidae"          
# [36] "Syngnathidae"       "Tetraodontidae"     "Mullidae"           "Latridae"           "Moridae"           
# [41] "Congridae"          "Diodontidae"        "Aplodactylidae"     "Scyliorhinidae"     "Enoplosidae"       
# [46] "Pentacerotidae"     "Scorpaenidae"       "Pataecidae"         "Loliginidae"        "Clinidae"          
# [51] "Sparidae"           "Hemiramphidae"      "Monodactylidae"     "Gerreidae"          "Hypnidae"          
# [56] "Rhinobatidae"       "Terapontidae"       "Sepiidae"           "Cheloniidae"        "Urolophidae"       
# [61] "Sillaginidae"       "Mugilidae"          "Arripidae"          "Sphyraenidae"       "Trachichthyidae"   
# [66] "Octopodidae"        "Dasyatidae"         "Cirrhitidae"        "Callanthiidae"      "Zanclidae"         
# [71] "Priacanthidae"      "Callionymidae"      "Muraenidae"         "Brachaeluridae"     "Scatophagidae"     
# [76] "Carcharhinidae"     "Fistulariidae"      "Plotosidae"         "Rajidae"            "Parascylliidae"    
# [81] "Heterodontidae"     "Chironemidae"       "Aulopidae"          "Otariidae"          "Balistidae"        
# [86] "Clupeidae"          "Cyttidae"           "Glaucosomatidae"    "Platycephalidae"    "Monocentridae"     
# [91] "Paralichthyidae"    "Batrachoididae"     "Hemiscylliidae"     "Aulostomidae"       "Belonidae"         
# [96] NA                   "Gobiesocidae"       "Ginglymostomatidae" "Myliobatidae"       "Odontaspididae"    
# [101] "Scombridae"         "Echeneidae"         "Kuhliidae"          "Elapidae"           "Opistognathidae"   
# [106] "Malacanthidae"      "Atherinidae"        "Bovichtidae"        "Creediidae"         "Sciaenidae"
nrow(unique(rls_2021_2022["species_name"]))
## output:
# [1] 1136
nrow(unique(rls_2021_2022["reporting_name"]))
## output:
# [1] 1131

# abundance related parameters
range(rls_2021_2022$size_class, na.rm = TRUE)
## output:
# [1] 0 300, we still have sharks and even mammals in this dataset
range(rls_2021_2022$total, na.rm = TRUE)
## output:
# [1] 0 11000, our counts have quite a big range
range(rls_2021_2022$biomass, na.rm = TRUE)
## output:
# [1] 0.0 499152.7

##### conclusion: irrelevant taxa must be removed from the dataset
##### we are going to use the species name as species if for further analysis,
##### but the official name needs to be validated

##############################################

##### summary

# we need to check the effect of blocks and depths on the community data
# and decide, how to average the data
# we will use the variables latitude and longitude for the site identification
# think about how the survey time affects the community data
# irrelevant taxa must be removed from the dataset
# get accepted names of our species names from WORMS

##### end