###### RLS DATA EXPLORATION 2019-2022

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# read RLS dataset
rls_2019_2022 = read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")
rls_2020_2022 = rls_2019_2022 %>% filter(survey_date > "2019-12-31")
# check variable names of the dataset
names(rls_2019_2022)

##### explore the location

unique(rls_2019_2022$country) 
## output: 
# "Australia" "New Caledonia"
unique(rls_2019_2022$area)
## output:
# [1] "Western Australia"  "Christmas Island"   "New South Wales"    "Northern Territory" "Tasmania"          
# [6] "Victoria"           "South Australia"    "Queensland"         "Other Territories"  "Noumea"
unique(rls_2019_2022$ecoregion)
## output:
# [1] "Exmouth to Broome"                         "Cocos-Keeling/Christmas Island"           
# [3] "Manning-Hawkesbury"                        "Bonaparte Coast"                          
# [5] "Arnhem Coast to Gulf of Carpenteria"       "Bassian"                                  
# [7] "South Australian Gulfs"                    "Cape Howe"                                
# [9] "Leeuwin"                                   "Central and Southern Great Barrier Reef"  
# [11] "Coral Sea"                                 "Tweed-Moreton"                            
# [13] "Ningaloo"                                  "Western Bassian"                          
# [15] "Lord Howe and Norfolk Islands"             "Houtman"                                  
# [17] "Torres Strait Northern Great Barrier Reef" "Gulf of Papua"                            
# [19] "Shark Bay"                                 "New Caledonia"
unique(rls_2019_2022$realm)
## output:
# [1] "Central Indo-Pacific"  "Temperate Australasia"
unique(rls_2019_2022$location)
## output:
# [1] "Ashmore/Cartier"            "Christmas Island"           "Sydney"                     "Scott & Seringapatam Reefs"
# [5] "Kimberley"                  "Arafura"                    "Arnhem"                     "Tinderbox"                 
# [9] "Ninepin Point"              "Port Phillip Heads"         "Port Phillip Bay"           "Port Stephens"             
# [13] "Encounter"                  "Port Davey"                 "Kangaroo Island"            "Gippsland Lakes"           
# [17] "Swansea"                    "Adelaide"                   "Maria Island"               "Bicheno"                   
# [21] "Rocky Cape"                 "Jervis Bay"                 "Geographe Bay"              "QLD inshore - South"       
# [25] "Coral Sea - Central"        "Tasmania - South East"      "D'Entrecasteaux & Derwent"  "GBR - South"               
# [29] "Rottnest Island"            "Solitary Islands"           "Ningaloo Reef"              "Exmouth"                   
# [33] "Batemans"                   "Coral Sea - South"          "Lord Howe Island"           "Shoalwater"                
# [37] "Yorke Peninsula"            "Marmion"                    "Wilsons Promontory"         "Beware Reef"               
# [41] "GBR - Central"              "QLD inshore - Central"      "Coral Sea - North"          "Coral Sea - Far North"     
# [45] "Torres Strait"              "West Cape York"             "QLD inshore - North"        "GBR - North"               
# [49] "Keppels"                    "Capricorn-Bunker"           "Norfolk Island"             "Flinders Island"           
# [53] "Abrolhos (WA)"              "Cape Howe"                  "Shark Bay"                  "Pilbara"                   
# [57] "Byron"                      "Moreton"                    "Great Sandy"                "Ningaloo Reef - North"     
# [61] "NSW - Central North"        "Wessel"                     "Darwin"                     "Oceanic Shoals"            
# [65] "Carpentaria"                "New Caledonia"              "Tweed Heads"                "Gold Coast"                
# [69] "Tasmania - South"           "Port Stephens - North" 

##### conclusion: the realm might be interesting in terms of species composition and distribution

##############################################

##### check survey identifier

length(unique(rls_2019_2022[["FID"]]))
## output:
# [1] 223568, this column contains an identifier for each survey
length(unique(rls_2019_2022[["survey_id"]]))
## output:
# [1] 3381
length(unique(rls_2019_2022[["site_code"]]))
## output:
# [1] 869
length(unique(rls_2019_2022[["site_name"]]))
## output:
# [1] 867
head(rls_2019_2022$geom)
length(unique(rls_2019_2022[["geom"]]))
## probably the specific coordinates of each observation
rls_2019_2022 %>% select(latitude, longitude, survey_date) %>% n_distinct()
## output:
# [1] 1249

##### conclusion: FID and survey_id can be dropped
##### the other ids might be important if we take depths and blocks into account...
##### if we merge the data from different blocks and depths, we have 901 sites

##############################################

##### check coordinates

# geographical range
range(rls_2019_2022$latitude, na.rm = TRUE)
## output:
# [1] -43.53  -9.88
range(rls_2019_2022$survey_latitude, na.rm = TRUE)
## output:
# [1] -43.53  -9.88
range(rls_2019_2022$longitude, na.rm = TRUE)
## output:
# [1] 105.67 167.99
range(rls_2019_2022$survey_longitude, na.rm = TRUE)
## output:
# [1] 113.74 167.96

# let's look at the first rows of survey_latitude and survey_longitude
head(rls_2019_2022["survey_latitude"])
head(rls_2019_2022["survey_longitude"])
## it seems that these columns have some NAs

# how many rows are in those columns without NAs?
nrow(rls_2019_2022) - nrow(rls_2019_2022[complete.cases(rls_2019_2022$survey_latitude),]) # output: [1] 139780
nrow(rls_2019_2022) - nrow(rls_2019_2022[complete.cases(rls_2019_2022$survey_longitude),]) # output: [1] 139780
# only 139780 of 223568 rows do not have NAs

##### conclusion: rather use latitude and longitude instead of survey_latitude and survey_longitude

##############################################

##### specific survey data

# time period and survey time
range(rls_2019_2022$survey_date, na.rm = TRUE)
## output:
# [1] "2019-01-01" "2022-09-15"
s_time = rls_2019_2022 %>% select(hour) %>% distinct()
## surveys were done between 12pm (midnight) and 17pm

# program and method
unique(rls_2019_2022$program)
## output:
# [1] "RLS"  "ATRC", what is ATRC?
unique(rls_2019_2022$method)
## output:
# [1] 1,  makes sense in view of the RLS manual (method 1 -> fish)
unique(rls_2019_2022$block)
## output:
# [1] 2 1, check RLS manual, blocks can be merged

# physicial parameters
range(rls_2019_2022$visibility, na.rm = TRUE)
## output:
# [1]  2 60
range(rls_2019_2022$depth, na.rm = TRUE)
## output:
# [1]  0.8 23, check later if depth has an effect on the community composition

##### conclusion: how does the survey time affect the community data?
##### data of different blocks can be merged
##### check, if depth is a predictor of our community data

##############################################

##### community data

# taxonomy
unique(rls_2019_2022$phylum)
## output:
# [1] "Chordata" "Mollusca" "Cnidaria"
unique(rls_2019_2022$class)
## output:
# [1] "Actinopterygii" "Elasmobranchii" "Reptilia" "Cephalopoda" "Scyphozoa" "Mammalia"
unique(rls_2019_2022$order)
## output:
# [1] "Perciformes"               "Tetraodontiformes"         "Myliobatiformes"           "Testudines"               
# [5] "Aulopiformes"              "Beryciformes"              "Syngnathiformes"           "Scorpaeniformes"          
# [9] "Squamata"                  "Atheriniformes"            "Clupeiformes"              "Anguilliformes"           
# [13] "Carcharhiniformes"         "Octopoda"                  "Orectolobiformes"          "Pectinoida"               
# [17] "Gadiformes"                "Heterodontiformes"         "Gobiesociformes"           "Rajiformes"               
# [21] "Teuthoidea"                "Sepioidea"                 "Rhizostomeae"              "Torpediniformes"          
# [25] "Carnivora"                 "Batrachoidiformes"         NA                          "Lamniformes"              
# [29] "Siluriformes"              "Beloniformes"              "Cetartiodactyla"           "Eupercaria incertae sedis"
# [33] "Zeiformes"                 "Pleuronectiformes"         "Myopsida"                  "Acanthuriformes"          
# [37] "Callionymiformes"          "Blenniiformes"             "Kurtiformes"               "Gasterosteiformes"        
# [41] "Cetacea" 
unique(rls_2019_2022$family)
## output:
# [1] "Serranidae"         "Scaridae"           "Pseudochromidae"    "Pomacentridae"      "Pomacanthidae"     
# [6] "Pinguipedidae"      "Nemipteridae"       "Labridae"           "Cirrhitidae"        "Chaetodontidae"    
# [11] "Acanthuridae"       "Balistidae"         "Zanclidae"          "Mullidae"           "Lutjanidae"        
# [16] "Kyphosidae"         "Carangidae"         "Monacanthidae"      "Lethrinidae"        "Blenniidae"        
# [21] "Siganidae"          "Dasyatidae"         "Tetraodontidae"     "Gobiidae"           "Ephippidae"        
# [26] "Cheloniidae"        "Ostraciidae"        "Apogonidae"         "Synodontidae"       "Caesionidae"       
# [31] "Holocentridae"      "Aulostomidae"       "Scorpaenidae"       "Pempheridae"        "Microdesmidae"     
# [36] "Elapidae"           "Atherinidae"        "Haemulidae"         "Clupeidae"          "Malacanthidae"     
# [41] "Plesiopidae"        "Gerreidae"          "Cheilodactylidae"   "Urolophidae"        "Sparidae"          
# [46] "Scombridae"         "Muraenidae"         "Tripterygiidae"     "Carcharhinidae"     "Octopodidae"       
# [51] "Fistulariidae"      "Orectolobidae"      "Myliobatidae"       "Glaucosomatidae"    "Diodontidae"       
# [56] "Odacidae"           "Latridae"           "Pentacerotidae"     "Dinolestidae"       "Moridae"           
# [61] "Heterodontidae"     "Aplodactylidae"     "Gobiesocidae"       "Echeneidae"         "Mugilidae"         
# [66] "Clinidae"           "Scyliorhinidae"     "Enoplosidae"        "Chironemidae"       "Rhinobatidae"      
# [71] "Monodactylidae"     "Aulopidae"          "Loliginidae"        "Sepiidae"           "Platycephalidae"   
# [76] "Parascylliidae"     "Ophichthidae"       "Bovichtidae"        "Catostylidae"       "Trachichthyidae"   
# [81] "Hypnidae"           "Otariidae"          "Creediidae"         "Batrachoididae"     "Arripidae"         
# [86] "Sphyraenidae"       "Callionymidae"      NA                   "Syngnathidae"       "Gempylidae"        
# [91] "Berycidae"          "Engraulidae"        "Odontaspididae"     "Sillaginidae"       "Latidae"           
# [96] "Terapontidae"       "Plotosidae"         "Hemiramphidae"      "Brachaeluridae"     "Priacanthidae"     
# [101] "Ginglymostomatidae" "Sepiadariidae"      "Ptereleotridae"     "Hemiscylliidae"     "Pomatomidae"       
# [106] "Congridae"          "Aracanidae"         "Delphinidae"        "Ambassidae"         "Pataecidae"        
# [111] "Callanthiidae"      "Scatophagidae"      "Rajidae"            "Cyttidae"           "Monocentridae"     
# [116] "Paralichthyidae"    "Belonidae"          "Kuhliidae"          "Opistognathidae"    "Centriscidae"      
# [121] "Sciaenidae" 
nrow(unique(rls_2019_2022["species_name"]))
## output:
# [1] 1358
nrow(unique(rls_2019_2022["reporting_name"]))
## output:
# [1] 1346

# abundance related parameters
range(rls_2019_2022$size_class, na.rm = TRUE)
## output:
# [1] 0 350, we still have sharks and even mammals in this dataset
range(rls_2019_2022$total, na.rm = TRUE)
## output:
# [1] 0 11000, our counts have quite a big range
range(rls_2019_2022$biomass, na.rm = TRUE)
## output:
# [1] 0.0 2015471

##### conclusion: irrelevant classes = ("Elasmobranchii", "Reptilia", "Cephalopoda", "Scyphozoa", "Mammalia") 
##### irrelevant classes must be removed from the dataset
##### we are going to use species_name as species for further analysis,
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