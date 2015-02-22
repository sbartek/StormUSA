    

# Data Processing #

## Load required libraries, set directories and fileahash ##


```r
require(data.table)
require(ggplot2)
require(filehash)
require(knitr)


if (!file.exists('data')) {dir.create('data')}
dbPath <- 'data/DB'
if (!file.exists(dbPath)) {dbCreate(dbPath)}
db <-  dbInit(dbPath)
```

## Download and read data ##



```r
download.storm.dt <- function() {
  fileURI <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
  fileBzPath <- 'data/repdata-data-StormData.csv.bz2'
  if (!file.exists(fileBzPath)) {
    download.file(fileURI, destfile=fileBzPath, method="curl")
  }
  storm.df <- read.csv(bzfile(fileBzPath))
  as.data.table(storm.df)
}


get.or.cache <- function(fun, svar) {
  if (dbExists(db, svar)) {
    #We check if svar is cached. If not we exectute function and cache
    #its value.

    print("Reading data.table form cache...")
    dbFetch(db, svar)
  } else {
    var <- fun()
    dbInsert(db, svar, var)
  }
}

storm.dt <- get.or.cache(download.storm.dt, "storm.dt")
```

```
## [1] "Reading data.table form cache..."
```

```r
str(storm.dt)
```

```
## Classes 'data.table' and 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "10/10/1954 0:00:00",..: 6523 6523 4213 11116 1426 1426 1462 2873 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "000","0000","00:00:00 AM",..: 212 257 2645 1563 2524 3126 122 1563 3126 3126 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "?","ABNORMALLY DRY",..: 830 830 830 830 830 830 830 830 830 830 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","E","Eas","EE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","?","(01R)AFB GNRY RNG AL",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","10/10/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels "","?","0000",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","(0E4)PAYSON ARPT",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels "","2","43","9V9",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels ""," ","  ","   ",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
evtype <- unique(tolower(storm.dt[,EVTYPE]))
evtype
```

```
##   [1] "tornado"                        "tstm wind"                     
##   [3] "hail"                           "freezing rain"                 
##   [5] "snow"                           "ice storm/flash flood"         
##   [7] "snow/ice"                       "winter storm"                  
##   [9] "hurricane opal/high winds"      "thunderstorm winds"            
##  [11] "record cold"                    "hurricane erin"                
##  [13] "hurricane opal"                 "heavy rain"                    
##  [15] "lightning"                      "thunderstorm wind"             
##  [17] "dense fog"                      "rip current"                   
##  [19] "thunderstorm wins"              "flash flood"                   
##  [21] "flash flooding"                 "high winds"                    
##  [23] "funnel cloud"                   "tornado f0"                    
##  [25] "thunderstorm winds lightning"   "thunderstorm winds/hail"       
##  [27] "heat"                           "wind"                          
##  [29] "lighting"                       "heavy rains"                   
##  [31] "lightning and heavy rain"       "funnel"                        
##  [33] "wall cloud"                     "flooding"                      
##  [35] "thunderstorm winds hail"        "flood"                         
##  [37] "cold"                           "heavy rain/lightning"          
##  [39] "flash flooding/thunderstorm wi" "wall cloud/funnel cloud"       
##  [41] "thunderstorm"                   "waterspout"                    
##  [43] "extreme cold"                   "hail 1.75)"                    
##  [45] "lightning/heavy rain"           "high wind"                     
##  [47] "blizzard"                       "blizzard weather"              
##  [49] "wind chill"                     "breakup flooding"              
##  [51] "high wind/blizzard"             "river flood"                   
##  [53] "heavy snow"                     "freeze"                        
##  [55] "coastal flood"                  "high wind and high tides"      
##  [57] "high wind/blizzard/freezing ra" "high tides"                    
##  [59] "high wind and heavy snow"       "record cold and high wind"     
##  [61] "record high temperature"        "record high"                   
##  [63] "high winds heavy rains"         "high wind/ blizzard"           
##  [65] "ice storm"                      "blizzard/high wind"            
##  [67] "high wind/low wind chill"       "heavy snow/high"               
##  [69] "record low"                     "high winds and wind chill"     
##  [71] "heavy snow/high winds/freezing" "low temperature record"        
##  [73] "avalanche"                      "marine mishap"                 
##  [75] "wind chill/high wind"           "high wind/wind chill/blizzard" 
##  [77] "high wind/wind chill"           "high wind/heavy snow"          
##  [79] "high temperature record"        "flood watch/"                  
##  [81] "record high temperatures"       "high wind/seas"                
##  [83] "high winds/heavy rain"          "high seas"                     
##  [85] "severe turbulence"              "record rainfall"               
##  [87] "record snowfall"                "record warmth"                 
##  [89] "heavy snow/wind"                "extreme heat"                  
##  [91] "wind damage"                    "dust storm"                    
##  [93] "apache county"                  "sleet"                         
##  [95] "hail storm"                     "funnel clouds"                 
##  [97] "flash floods"                   "dust devil"                    
##  [99] "excessive heat"                 "thunderstorm winds/funnel clou"
## [101] "winter storm/high wind"         "winter storm/high winds"       
## [103] "gusty winds"                    "strong winds"                  
## [105] "flooding/heavy rain"            "snow and wind"                 
## [107] "heavy surf coastal flooding"    "heavy surf"                    
## [109] "heavy precipatation"            "urban flooding"                
## [111] "high surf"                      "blowing dust"                  
## [113] "urban/small"                    "wild fires"                    
## [115] "high"                           "urban/small flooding"          
## [117] "water spout"                    "high winds dust storm"         
## [119] "winter storm high winds"        "local flood"                   
## [121] "winter storms"                  "mudslides"                     
## [123] "rainstorm"                      "severe thunderstorm"           
## [125] "severe thunderstorms"           "severe thunderstorm winds"     
## [127] "thunderstorms winds"            "dry microburst"                
## [129] "flood/flash flood"              "flood/rain/winds"              
## [131] "winds"                          "dry microburst 61"             
## [133] "thunderstorms"                  "flash flood winds"             
## [135] "urban/small stream flooding"    "microburst"                    
## [137] "strong wind"                    "high wind damage"              
## [139] "stream flooding"                "urban and small"               
## [141] "heavy snowpack"                 "ice"                           
## [143] "flash flood/"                   "downburst"                     
## [145] "gustnado and"                   "flood/rain/wind"               
## [147] "wet microburst"                 "downburst winds"               
## [149] "dry microburst winds"           "dry mircoburst winds"          
## [151] "dry microburst 53"              "small stream urban flood"      
## [153] "microburst winds"               "high winds 57"                 
## [155] "dry microburst 50"              "high winds 66"                 
## [157] "high winds 76"                  "high winds 63"                 
## [159] "high winds 67"                  "blizzard/heavy snow"           
## [161] "heavy snow/high winds"          "blowing snow"                  
## [163] "high winds 82"                  "high winds 80"                 
## [165] "high winds 58"                  "freezing drizzle"              
## [167] "lightning thunderstorm windss"  "dry microburst 58"             
## [169] "hail 75"                        "high winds 73"                 
## [171] "high winds 55"                  "light snow and sleet"          
## [173] "urban flood"                    "dry microburst 84"             
## [175] "thunderstorm winds 60"          "heavy rain/flooding"           
## [177] "thunderstorm windss"            "tornados"                      
## [179] "glaze"                          "record heat"                   
## [181] "coastal flooding"               "heat wave"                     
## [183] "first snow"                     "freezing rain and sleet"       
## [185] "unseasonably dry"               "unseasonably wet"              
## [187] "wintry mix"                     "winter weather"                
## [189] "unseasonably cold"              "extreme/record cold"           
## [191] "rip currents heavy surf"        "sleet/rain/snow"               
## [193] "unseasonably warm"              "drought"                       
## [195] "normal precipitation"           "high winds/flooding"           
## [197] "dry"                            "rain/snow"                     
## [199] "snow/rain/sleet"                "waterspout/tornado"            
## [201] "waterspouts"                    "waterspout tornado"            
## [203] "urban/small stream flood"       "storm surge"                   
## [205] "waterspout-tornado"             "waterspout-"                   
## [207] "tornadoes, tstm wind, hail"     "tropical storm alberto"        
## [209] "tropical storm"                 "tropical storm gordon"         
## [211] "tropical storm jerry"           "lightning thunderstorm winds"  
## [213] "wayterspout"                    "minor flooding"                
## [215] "lightning injury"               "urban/small stream  flood"     
## [217] "lightning and thunderstorm win" "thunderstorm winds53"          
## [219] "urban and small stream flood"   "urban and small stream"        
## [221] "wildfire"                       "damaging freeze"               
## [223] "thunderstorm winds 13"          "small hail"                    
## [225] "heavy snow/high wind"           "hurricane"                     
## [227] "wild/forest fire"               "small stream flooding"         
## [229] "mud slide"                      "ligntning"                     
## [231] "frost"                          "freezing rain/snow"            
## [233] "high winds/"                    "thundersnow"                   
## [235] "floods"                         "extreme wind chills"           
## [237] "cool and wet"                   "heavy rain/snow"               
## [239] "small stream and urban floodin" "small stream/urban flood"      
## [241] "snow/sleet/freezing rain"       "severe cold"                   
## [243] "glaze ice"                      "cold wave"                     
## [245] "early snow"                     "small stream and urban flood"  
## [247] "high  winds"                    "rural flood"                   
## [249] "small stream and"               "mud slides"                    
## [251] "hail 80"                        "extreme wind chill"            
## [253] "cold and wet conditions"        "excessive wetness"             
## [255] "gradient winds"                 "heavy snow/blowing snow"       
## [257] "sleet/ice storm"                "thunderstorm winds urban flood"
## [259] "thunderstorm winds small strea" "rotating wall cloud"           
## [261] "large wall cloud"               "cold air funnel"               
## [263] "gustnado"                       "cold air funnels"              
## [265] "blowing snow- extreme wind chi" "snow and heavy snow"           
## [267] "ground blizzard"                "major flood"                   
## [269] "snow/heavy snow"                "freezing rain/sleet"           
## [271] "ice jam flooding"               "snow- high wind- wind chill"   
## [273] "street flood"                   "cold air tornado"              
## [275] "small stream flood"             "fog"                           
## [277] "thunderstorm winds 2"           "funnel cloud/hail"             
## [279] "ice/snow"                       "tstm wind 51"                  
## [281] "tstm wind 50"                   "tstm wind 52"                  
## [283] "tstm wind 55"                   "heavy snow/blizzard"           
## [285] "thunderstorm winds 61"          "hail 0.75"                     
## [287] "thunderstorm damage"            "thundertorm winds"             
## [289] "hail 1.00"                      "hail/winds"                    
## [291] "snow and ice"                   "wind storm"                    
## [293] "snowstorm"                      "grass fires"                   
## [295] "lake flood"                     "prolong cold"                  
## [297] "hail/wind"                      "hail 1.75"                     
## [299] "thunderstormw 50"               "wind/hail"                     
## [301] "snow and ice storm"             "urban and small stream floodin"
## [303] "thunderstorms wind"             "thunderstorm  winds"           
## [305] "heavy snow/sleet"               "agricultural freeze"           
## [307] "drought/excessive heat"         "tunderstorm wind"              
## [309] "tropical storm dean"            "thundertsorm wind"             
## [311] "thunderstorm winds/ hail"       "thunderstorm wind/lightning"   
## [313] "heavy rain/severe weather"      "thundestorm winds"             
## [315] "waterspout/ tornado"            "lightning."                    
## [317] "warm dry conditions"            "hurricane-generated swells"    
## [319] "heavy snow/ice storm"           "river and stream flood"        
## [321] "high wind 63"                   "coastal surge"                 
## [323] "heavy snow and ice storm"       "minor flood"                   
## [325] "high winds/coastal flood"       "rain"                          
## [327] "river flooding"                 "snow/rain"                     
## [329] "ice floes"                      "high waves"                    
## [331] "snow squalls"                   "snow squall"                   
## [333] "thunderstorm wind g50"          "lightning fire"                
## [335] "blizzard/freezing rain"         "heavy lake snow"               
## [337] "heavy snow/freezing rain"       "lake effect snow"              
## [339] "heavy wet snow"                 "dust devil waterspout"         
## [341] "thunderstorm winds/heavy rain"  "thunderstrom winds"            
## [343] "thunderstorm winds      le cen" "hail 225"                      
## [345] "blizzard and heavy snow"        "heavy snow and ice"            
## [347] "ice storm and snow"             "heavy snow andblowing snow"    
## [349] "heavy snow/ice"                 "blizzard and extreme wind chil"
## [351] "low wind chill"                 "blowing snow & extreme wind ch"
## [353] "waterspout/"                    "urban/small stream"            
## [355] "tornado f3"                     "funnel cloud."                 
## [357] "torndao"                        "hail 0.88"                     
## [359] "flood/river flood"              "mud slides urban flooding"     
## [361] "tornado f1"                     "thunderstorm winds g"          
## [363] "deep hail"                      "glaze/ice storm"               
## [365] "heavy snow/winter storm"        "avalance"                      
## [367] "blizzard/winter storm"          "dust storm/high winds"         
## [369] "ice jam"                        "forest fires"                  
## [371] "thunderstorm wind g60"          "frost\\freeze"                 
## [373] "thunderstorm winds."            "hail 88"                       
## [375] "hail 175"                       "hvy rain"                      
## [377] "hail 100"                       "hail 150"                      
## [379] "hail 075"                       "thunderstorm wind g55"         
## [381] "hail 125"                       "thunderstorm winds g60"        
## [383] "hard freeze"                    "hail 200"                      
## [385] "thunderstorm winds funnel clou" "thunderstorm winds 62"         
## [387] "wildfires"                      "record heat wave"              
## [389] "heavy snow and high winds"      "heavy snow/high winds & flood" 
## [391] "hail flooding"                  "thunderstorm winds/flash flood"
## [393] "high wind 70"                   "wet snow"                      
## [395] "heavy rain and flood"           "local flash flood"             
## [397] "thunderstorm winds 53"          "flood/flash flooding"          
## [399] "tornado/waterspout"             "rain and wind"                 
## [401] "thunderstorm wind 59"           "thunderstorm wind 52"          
## [403] "coastal/tidal flood"            "snow/ice storm"                
## [405] "below normal precipitation"     "rip currents/heavy surf"       
## [407] "flash flood/flood"              "excessive rain"                
## [409] "record/excessive heat"          "heat waves"                    
## [411] "light snow"                     "thunderstorm wind 69"          
## [413] "hail damage"                    "lightning damage"              
## [415] "record temperatures"            "lightning and winds"           
## [417] "fog and cold temperatures"      "other"                         
## [419] "record snow"                    "snow/cold"                     
## [421] "flash flood from ice jams"      "tstm wind g58"                 
## [423] "mudslide"                       "heavy snow squalls"            
## [425] "heavy snow/squalls"             "heavy snow-squalls"            
## [427] "icy roads"                      "heavy mix"                     
## [429] "snow freezing rain"             "lack of snow"                  
## [431] "snow/sleet"                     "snow/freezing rain"            
## [433] "snow drought"                   "thunderstormw winds"           
## [435] "thunderstorm wind 60 mph"       "thunderstorm wind 65mph"       
## [437] "thunderstorm wind/ trees"       "thunderstorm wind/awning"      
## [439] "thunderstorm wind 98 mph"       "thunderstorm wind trees"       
## [441] "torrential rain"                "tornado f2"                    
## [443] "rip currents"                   "hurricane emily"               
## [445] "hurricane gordon"               "hurricane felix"               
## [447] "thunderstorm wind 59 mph"       "thunderstorm winds 63 mph"     
## [449] "thunderstorm wind/ tree"        "thunderstorm damage to"        
## [451] "thunderstorm wind 65 mph"       "flash flood - heavy rain"      
## [453] "thunderstorm wind."             "flash flood/ street"           
## [455] "thunderstorm wind 59 mph."      "heavy snow   freezing rain"    
## [457] "dam failure"                    "thunderstorm hail"             
## [459] "hail 088"                       "thunderstorm windshail"        
## [461] "lightning  wauseon"             "thuderstorm winds"             
## [463] "ice and snow"                   "record cold/frost"             
## [465] "storm force winds"              "freezing rain and snow"        
## [467] "freezing rain sleet and"        "southeast"                     
## [469] "heavy snow & ice"               "freezing drizzle and freezing" 
## [471] "thunderstorm winds and"         "hail/icy roads"                
## [473] "flash flood/heavy rain"         "heavy rain; urban flood winds;"
## [475] "heavy precipitation"            "tstm wind damage"              
## [477] "high water"                     "flood flash"                   
## [479] "rain/wind"                      "thunderstorm winds 50"         
## [481] "thunderstorm wind g52"          "flood flood/flash"             
## [483] "thunderstorm winds 52"          "snow showers"                  
## [485] "thunderstorm wind g51"          "heat wave drought"             
## [487] "heavy snow/blizzard/avalanche"  "record snow/cold"              
## [489] "wet weather"                    "unseasonably warm and dry"     
## [491] "freezing rain sleet and light"  "record/excessive rainfall"     
## [493] "tidal flood"                    "beach erosin"                  
## [495] "thunderstorm wind g61"          "flood/flash"                   
## [497] "low temperature"                "sleet & freezing rain"         
## [499] "heavy rains/flooding"           "thunderestorm winds"           
## [501] "thunderstorm winds/flooding"    "thundeerstorm winds"           
## [503] "highway flooding"               "thunderstorm w inds"           
## [505] "hypothermia"                    "flash flood/ flood"            
## [507] "thunderstorm wind 50"           "thunerstorm winds"             
## [509] "heavy rain/mudslides/flood"     "mud/rock slide"                
## [511] "high winds/cold"                "beach erosion/coastal flood"   
## [513] "cold/winds"                     "snow/ bitter cold"             
## [515] "thunderstorm wind 56"           "snow sleet"                    
## [517] "dry hot weather"                "cold weather"                  
## [519] "rapidly rising water"           "hail aloft"                    
## [521] "early freeze"                   "ice/strong winds"              
## [523] "extreme wind chill/blowing sno" "snow/high winds"               
## [525] "high winds/snow"                "early frost"                   
## [527] "snowmelt flooding"              "heavy snow and strong winds"   
## [529] "snow accumulation"              "blowing snow/extreme wind chil"
## [531] "snow/ ice"                      "snow/blowing snow"             
## [533] "tornadoes"                      "thunderstorm wind/hail"        
## [535] "flash flooding/flood"           "hail 275"                      
## [537] "hail 450"                       "flash floooding"               
## [539] "excessive rainfall"             "thunderstormw"                 
## [541] "hailstorm"                      "tstm winds"                    
## [543] "beach flood"                    "hailstorms"                    
## [545] "tstmw"                          "funnels"                       
## [547] "tstm wind 65)"                  "thunderstorm winds/ flood"     
## [549] "heavy rainfall"                 "heat/drought"                  
## [551] "heat drought"                   "near record snow"              
## [553] "landslide"                      "high wind and seas"            
## [555] "thunderstormwinds"              "thunderstorm winds heavy rain" 
## [557] "sleet/snow"                     "excessive"                     
## [559] "snow/sleet/rain"                "wild/forest fires"             
## [561] "heavy seas"                     "duststorm"                     
## [563] "flood & heavy rain"             "?"                             
## [565] "thunderstrom wind"              "flood/flashflood"              
## [567] "snow and cold"                  "hot pattern"                   
## [569] "prolong cold/snow"              "brush fires"                   
## [571] "snow\\cold"                     "winter mix"                    
## [573] "excessive precipitation"        "snowfall record"               
## [575] "hot/dry pattern"                "dry pattern"                   
## [577] "mild/dry pattern"               "mild pattern"                  
## [579] "landslides"                     "heavy showers"                 
## [581] "heavy snow and"                 "high wind 48"                  
## [583] "lake-effect snow"               "brush fire"                    
## [585] "waterspout funnel cloud"        "urban small stream flood"      
## [587] "saharan dust"                   "heavy shower"                  
## [589] "urban flood landslide"          "heavy swells"                  
## [591] "urban small"                    "urban floods"                  
## [593] "small stream"                   "heavy rain/urban flood"        
## [595] "flash flood/landslide"          "landslide/urban flood"         
## [597] "heavy rain/small stream urban"  "flash flood landslides"        
## [599] "extreme windchill"              "urban/sml stream fld"          
## [601] "tstm wind/hail"                 "record dry month"              
## [603] "temperature record"             "ice jam flood (minor"          
## [605] "rough surf"                     "marine accident"               
## [607] "coastal storm"                  "coastalflood"                  
## [609] "erosion/cstl flood"             "heavy rain and wind"           
## [611] "light snow/flurries"            "wet month"                     
## [613] "wet year"                       "tidal flooding"                
## [615] "beach erosion"                  "hot and dry"                   
## [617] "heavy rain/high surf"           "rain damage"                   
## [619] "unseasonable cold"              "street flooding"               
## [621] "ice fog"                        "excessive cold"                
## [623] "torrential rainfall"            "landslump"                     
## [625] "late-season snowfall"           "hurricane edouard"             
## [627] "heavy rain/wind"                "record warm temps."            
## [629] "extended cold"                  "freezing fog"                  
## [631] "drifting snow"                  "whirlwind"                     
## [633] "heavy snow shower"              "late snow"                     
## [635] "record may snow"                "record winter snow"            
## [637] " coastal flood"                 "record temperature"            
## [639] "late season snowfall"           "gusty wind"                    
## [641] "mixed precip"                   "black ice"                     
## [643] "gradient wind"                  "freezing spray"                
## [645] "summary jan 17"                 "summary of march 14"           
## [647] "summary of march 23"            "summary of march 24"           
## [649] "summary of april 3rd"           "summary of april 12"           
## [651] "summary of april 13"            "summary of april 21"           
## [653] "summary august 11"              "summary of april 27"           
## [655] "summary of may 9-10"            "summary of may 10"             
## [657] "summary of may 13"              "summary of may 14"             
## [659] "summary of may 22 am"           "summary of may 22 pm"          
## [661] "heatburst"                      "summary of may 26 am"          
## [663] "summary of may 26 pm"           "metro storm, may 26"           
## [665] "summary of may 31 am"           "summary of may 31 pm"          
## [667] "summary of june 3"              "summary of june 4"             
## [669] "summary june 5-6"               "summary june 6"                
## [671] "summary of june 11"             "summary of june 12"            
## [673] "summary of june 13"             "summary of june 15"            
## [675] "summary of june 16"             "summary june 18-19"            
## [677] "summary of june 23"             "summary of june 24"            
## [679] "summary of june 30"             "summary of july 2"             
## [681] "summary of july 3"              "summary of july 11"            
## [683] "summary of july 22"             "summary july 23-24"            
## [685] "summary of july 26"             "summary of july 29"            
## [687] "summary of august 1"            "summary august 2-3"            
## [689] "summary august 7"               "summary august 9"              
## [691] "summary august 10"              "summary august 17"             
## [693] "summary august 21"              "summary august 28"             
## [695] "summary september 4"            "summary september 20"          
## [697] "summary september 23"           "summary sept. 25-26"           
## [699] "summary: oct. 20-21"            "summary: october 31"           
## [701] "summary: nov. 6-7"              "summary: nov. 16"              
## [703] "wet micoburst"                  "hail(0.75)"                    
## [705] "no severe weather"              "summary of may 22"             
## [707] "summary of june 6"              "summary august 4"              
## [709] "summary of june 10"             "summary of june 18"            
## [711] "summary september 3"            "summary: sept. 18"             
## [713] "light snowfall"                 "gusty wind/rain"               
## [715] "gusty wind/hvy rain"            "early snowfall"                
## [717] "monthly snowfall"               "seasonal snowfall"             
## [719] "monthly rainfall"               "cold temperature"              
## [721] "sml stream fld"                 "mudslide/landslide"            
## [723] "volcanic ash"                   "volcanic ash plume"            
## [725] "thundersnow shower"             "none"                          
## [727] "cold and snow"                  "dam break"                     
## [729] "tstm wind (g45)"                "sleet/freezing rain"           
## [731] "blow-out tides"                 "unseasonably cool"             
## [733] "tstm heavy rain"                "tstm wind 40"                  
## [735] "tstm wind 45"                   "tstm wind (41)"                
## [737] "tstm wind (g40)"                "tstm wnd"                      
## [739] " tstm wind"                     "frost/freeze"                  
## [741] "rain (heavy)"                   "cold and frost"                
## [743] "urban/sml stream fldg"          "strong wind gust"              
## [745] "late freeze"                    "blow-out tide"                 
## [747] "hypothermia/exposure"           "mixed precipitation"           
## [749] "coastalstorm"                   "snow and sleet"                
## [751] "blizzard summary"               "summary of march 24-25"        
## [753] "summary of march 27"            "summary of march 29"           
## [755] "icestorm/blizzard"              "flood/strong wind"             
## [757] "tstm wind and lightning"        "mountain snows"                
## [759] "urban/small strm fldg"          "heavy surf and wind"           
## [761] "mild and dry pattern"           "typhoon"                       
## [763] "high swells"                    "high  swells"                  
## [765] "dry spell"                      " lightning"                    
## [767] "unseasonal rain"                "early rain"                    
## [769] "prolonged rain"                 "wintery mix"                   
## [771] "coastal flooding/erosion"       "hot spell"                     
## [773] "unseasonably hot"               " tstm wind (g45)"              
## [775] "tstm wind  (g45)"               "high wind (g40)"               
## [777] "tstm wind (g35)"                "dry weather"                   
## [779] "abnormal warmth"                "unusual warmth"                
## [781] "wake low wind"                  "cold temperatures"             
## [783] "cold wind chill temperatures"   "moderate snow"                 
## [785] "moderate snowfall"              "urban/street flooding"         
## [787] "coastal erosion"                "unusual/record warmth"         
## [789] "bitter wind chill"              "bitter wind chill temperatures"
## [791] "seiche"                         "tstm"                          
## [793] "coastal  flooding/erosion"      "unseasonably warm year"        
## [795] "hyperthermia/exposure"          "rock slide"                    
## [797] "ice pellets"                    "patchy dense fog"              
## [799] "record cool"                    "record warm"                   
## [801] "hot weather"                    "tropical depression"           
## [803] "volcanic eruption"              "cool spell"                    
## [805] "wind advisory"                  "gusty wind/hail"               
## [807] "red flag fire wx"               "first frost"                   
## [809] "excessively dry"                "light snow/freezing precip"    
## [811] "vog"                            "monthly precipitation"         
## [813] "monthly temperature"            "record dryness"                
## [815] "extreme windchill temperatures" "dry conditions"                
## [817] "remnants of floyd"              "landspout"                     
## [819] "driest month"                   "record  cold"                  
## [821] "late season hail"               "excessive snow"                
## [823] "dryness"                        "flood/flash/flood"             
## [825] "wind and wave"                  "light freezing rain"           
## [827] " wind"                          "record precipitation"          
## [829] "ice roads"                      "rough seas"                    
## [831] "unseasonably warm/wet"          "unseasonably cool & wet"       
## [833] "unusually warm"                 "tstm wind g45"                 
## [835] "non severe hail"                "non-severe wind damage"        
## [837] "unusually cold"                 "warm weather"                  
## [839] "thunderstorm wind (g40)"        "unseasonably warm & wet"       
## [841] " flash flood"                   "locally heavy rain"            
## [843] "wind gusts"                     "unseasonal low temp"           
## [845] "high surf advisory"             "late season snow"              
## [847] "gusty lake wind"                "abnormally dry"                
## [849] "winter weather mix"             "red flag criteria"             
## [851] "wnd"                            "cstl flooding/erosion"         
## [853] "smoke"                          " waterspout"                   
## [855] "snow advisory"                  "extremely wet"                 
## [857] "unusually late snow"            "very dry"                      
## [859] "record low rainfall"            "rogue wave"                    
## [861] "prolong warmth"                 "accumulated snowfall"          
## [863] "falling snow/ice"               "dust devel"                    
## [865] "non-tstm wind"                  "non tstm wind"                 
## [867] "gusty thunderstorm winds"       "patchy ice"                    
## [869] "heavy rain effects"             "excessive heat/drought"        
## [871] "northern lights"                "marine tstm wind"              
## [873] "   high surf advisory"          "hazardous surf"                
## [875] "winter weather/mix"             "astronomical high tide"        
## [877] "very warm"                      "abnormally wet"                
## [879] "tornado debris"                 "extreme cold/wind chill"       
## [881] "ice on road"                    "drowning"                      
## [883] "gusty thunderstorm wind"        "marine hail"                   
## [885] "high surf advisories"           "hurricane/typhoon"             
## [887] "heavy surf/high surf"           "sleet storm"                   
## [889] "storm surge/tide"               "cold/wind chill"               
## [891] "marine high wind"               "tsunami"                       
## [893] "dense smoke"                    "lakeshore flood"               
## [895] "marine thunderstorm wind"       "marine strong wind"            
## [897] "astronomical low tide"          "volcanic ashfall"
```



```r
strom.col.ordered <- function(cname) {
  sco <- storm.dt[,sum(get(cname)),by=EVTYPE]
  sco[order(sco$V1, decreasing=T)]
}

fatalities<-strom.col.ordered("FATALITIES")
cnum <- dim(fatalities)[1]
sum(fatalities[21:cnum]$V1)
```

```
## [1] 1632
```

```r
fatalities[1:20]
```

```
##                      EVTYPE   V1
##  1:                 TORNADO 5633
##  2:          EXCESSIVE HEAT 1903
##  3:             FLASH FLOOD  978
##  4:                    HEAT  937
##  5:               LIGHTNING  816
##  6:               TSTM WIND  504
##  7:                   FLOOD  470
##  8:             RIP CURRENT  368
##  9:               HIGH WIND  248
## 10:               AVALANCHE  224
## 11:            WINTER STORM  206
## 12:            RIP CURRENTS  204
## 13:               HEAT WAVE  172
## 14:            EXTREME COLD  160
## 15:       THUNDERSTORM WIND  133
## 16:              HEAVY SNOW  127
## 17: EXTREME COLD/WIND CHILL  125
## 18:             STRONG WIND  103
## 19:                BLIZZARD  101
## 20:               HIGH SURF  101
```

```r
injuries<-strom.col.ordered("INJURIES")
cnum <- dim(injuries)[1]
sum(injuries[21:cnum]$V1)
```

```
## [1] 5883
```

```r
sum(injuries[1:20]$V1)
```

```
## [1] 134645
```

```r
injuries[1:20]
```

```
##                 EVTYPE    V1
##  1:            TORNADO 91346
##  2:          TSTM WIND  6957
##  3:              FLOOD  6789
##  4:     EXCESSIVE HEAT  6525
##  5:          LIGHTNING  5230
##  6:               HEAT  2100
##  7:          ICE STORM  1975
##  8:        FLASH FLOOD  1777
##  9:  THUNDERSTORM WIND  1488
## 10:               HAIL  1361
## 11:       WINTER STORM  1321
## 12:  HURRICANE/TYPHOON  1275
## 13:          HIGH WIND  1137
## 14:         HEAVY SNOW  1021
## 15:           WILDFIRE   911
## 16: THUNDERSTORM WINDS   908
## 17:           BLIZZARD   805
## 18:                FOG   734
## 19:   WILD/FOREST FIRE   545
## 20:         DUST STORM   440
```


```r
propexps <- storm.dt[,PROPDMGEXP]
cropexps <- storm.dt[,CROPDMGEXP]
propexps.unique <- sort(as.character(unique(propexps)))
cropexps.unique <- sort(as.character(unique(cropexps)))
propexps.unique
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
cropexps.unique
```

```
## [1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
```

```r
tonum <- function(ch) {
  ch <- as.character(ch)
  if (ch=='h' || ch =='H') {
    1
  } else if (ch == 'K'||ch == 'k') {
    3
  } else if (ch=='m'|| ch=='M') {
    6
  }
  else if (ch=='B') {
    9
  } else if (ch %in% c("",  "-", "?", "+")) {
    0
  } else {
    as.integer(ch)
  }
}

propexps.new <- get.or.cache(function() {sapply(propexps, tonum)}, "propexps.new")
```

```
## [1] "Reading data.table form cache..."
```

```r
cropexps.new <- get.or.cache(function() {sapply(cropexps, tonum)}, "cropexps.new")
```

```
## [1] "Reading data.table form cache..."
```

```r
storm.dt[, PropExp:=propexps.new]
```

```
##         STATE__           BGN_DATE    BGN_TIME TIME_ZONE COUNTY
##      1:       1  4/18/1950 0:00:00        0130       CST     97
##      2:       1  4/18/1950 0:00:00        0145       CST      3
##      3:       1  2/20/1951 0:00:00        1600       CST     57
##      4:       1   6/8/1951 0:00:00        0900       CST     89
##      5:       1 11/15/1951 0:00:00        1500       CST     43
##     ---                                                        
## 902293:      56 11/30/2011 0:00:00 10:30:00 PM       MST      7
## 902294:      30 11/10/2011 0:00:00 02:48:00 PM       MST      9
## 902295:       2  11/8/2011 0:00:00 02:58:00 PM       AKS    213
## 902296:       2  11/9/2011 0:00:00 10:21:00 AM       AKS    202
## 902297:       1 11/28/2011 0:00:00 08:00:00 PM       CST      6
##           COUNTYNAME STATE     EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI
##      1:       MOBILE    AL    TORNADO         0                   
##      2:      BALDWIN    AL    TORNADO         0                   
##      3:      FAYETTE    AL    TORNADO         0                   
##      4:      MADISON    AL    TORNADO         0                   
##      5:      CULLMAN    AL    TORNADO         0                   
##     ---                                                           
## 902293: WYZ007 - 017    WY  HIGH WIND         0                   
## 902294: MTZ009 - 010    MT  HIGH WIND         0                   
## 902295:       AKZ213    AK  HIGH WIND         0                   
## 902296:       AKZ202    AK   BLIZZARD         0                   
## 902297:       ALZ006    AL HEAVY SNOW         0                   
##                   END_DATE    END_TIME COUNTY_END COUNTYENDN END_RANGE
##      1:                                         0         NA         0
##      2:                                         0         NA         0
##      3:                                         0         NA         0
##      4:                                         0         NA         0
##      5:                                         0         NA         0
##     ---                                                               
## 902293: 11/30/2011 0:00:00 10:30:00 PM          0         NA         0
## 902294: 11/10/2011 0:00:00 02:48:00 PM          0         NA         0
## 902295:  11/9/2011 0:00:00 01:15:00 PM          0         NA         0
## 902296:  11/9/2011 0:00:00 05:00:00 PM          0         NA         0
## 902297: 11/29/2011 0:00:00 04:00:00 AM          0         NA         0
##         END_AZI END_LOCATI LENGTH WIDTH  F MAG FATALITIES INJURIES PROPDMG
##      1:                      14.0   100  3   0          0       15    25.0
##      2:                       2.0   150  2   0          0        0     2.5
##      3:                       0.1   123  2   0          0        2    25.0
##      4:                       0.0   100  2   0          0        2     2.5
##      5:                       0.0   150  2   0          0        2     2.5
##     ---                                                                   
## 902293:                       0.0     0 NA  66          0        0     0.0
## 902294:                       0.0     0 NA  52          0        0     0.0
## 902295:                       0.0     0 NA  81          0        0     0.0
## 902296:                       0.0     0 NA   0          0        0     0.0
## 902297:                       0.0     0 NA   0          0        0     0.0
##         PROPDMGEXP CROPDMG CROPDMGEXP WFO                STATEOFFIC
##      1:          K       0                                         
##      2:          K       0                                         
##      3:          K       0                                         
##      4:          K       0                                         
##      5:          K       0                                         
##     ---                                                            
## 902293:          K       0          K RIW WYOMING, Central and West
## 902294:          K       0          K TFX          MONTANA, Central
## 902295:          K       0          K AFG          ALASKA, Northern
## 902296:          K       0          K AFG          ALASKA, Northern
## 902297:          K       0          K HUN            ALABAMA, North
##                                                                                ZONENAMES
##      1:                                                                                 
##      2:                                                                                 
##      3:                                                                                 
##      4:                                                                                 
##      5:                                                                                 
##     ---                                                                                 
## 902293: OWL CREEK & BRIDGER MOUNTAINS - OWL CREEK & BRIDGER MOUNTAINS - WIND RIVER BASIN
## 902294:        NORTH ROCKY MOUNTAIN FRONT - NORTH ROCKY MOUNTAIN FRONT - EASTERN GLACIER
## 902295:                    ST LAWRENCE IS. BERING STRAIT - ST LAWRENCE IS. BERING STRAIT
## 902296:                                    NORTHERN ARCTIC COAST - NORTHERN ARCTIC COAST
## 902297:                                                                MADISON - MADISON
##         LATITUDE LONGITUDE LATITUDE_E LONGITUDE_
##      1:     3040      8812       3051       8806
##      2:     3042      8755          0          0
##      3:     3340      8742          0          0
##      4:     3458      8626          0          0
##      5:     3412      8642          0          0
##     ---                                         
## 902293:        0         0          0          0
## 902294:        0         0          0          0
## 902295:        0         0          0          0
## 902296:        0         0          0          0
## 902297:        0         0          0          0
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         REMARKS
##      1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      2:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      3:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      4:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      5:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##     ---                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
## 902293:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           EPISODE NARRATIVE: A strong cold front moved south through north central Wyoming bringing high wind to the Meeteetse area and along the south slopes of the western Owl Creek Range. Wind gusts to 76 mph were recorded at Madden Reservoir.EVENT NARRATIVE: 
## 902294:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      EPISODE NARRATIVE: A strong westerly flow aloft produced gusty winds at the surface along the Rocky Mountain front and over the plains of Central Montana. Wind gusts in excess of 60 mph were reported.EVENT NARRATIVE: A wind gust to 60 mph was reported at East Glacier Park 1ENE (the Two Medicine DOT site).
## 902295: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902296: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902297:                           EPISODE NARRATIVE: An intense upper level low developed on the 28th at the base of a highly amplified upper trough across the Great Lakes and Mississippi Valley.  The upper low closed off over the mid South and tracked northeast across the Tennessee Valley during the morning of the 29th.   A warm conveyor belt of heavy rainfall developed in advance of the low which dumped from around 2 to over 5 inches of rain across the eastern two thirds of north Alabama and middle Tennessee.  The highest rain amounts were recorded in Jackson and DeKalb Counties with 3 to 5 inches.  The rain fell over 24 to 36 hour period, with rainfall remaining light to moderate during most its duration.  The rainfall resulted in minor river flooding along the Little River, Big Wills Creek and Paint Rock.   A landslide occurred on Highway 35 just north of Section in Jackson County.  A driver was trapped in his vehicle, but was rescued unharmed.  Trees, boulders and debris blocked 100 to 250 yards of Highway 35.\n\nThe rain mixed with and changed to snow across north Alabama during the afternoon and  evening hours of the 28th, and lasted into the 29th.  The heaviest bursts of snow occurred in northwest Alabama during the afternoon and evening hours, and in north central and northeast Alabama during the overnight and morning hours.  Since ground temperatures were in the 50s, and air temperatures in valley areas only dropped into the mid 30s, most of the snowfall melted on impact with mostly trace amounts reported in valley locations.  However, above 1500 foot elevation, snow accumulations of 1 to 2 inches were reported.  The heaviest amount was 2.3 inches on Monte Sano Mountain, about 5 miles northeast of Huntsville.EVENT NARRATIVE: Snowfall accumulations of up to 2.3 inches were reported on the higher elevations of eastern Madison County.  A snow accumulation of 1.5 inches was reported 2.7 miles south of Gurley, while 2.3 inches was reported 3 miles east of Huntsville atop Monte Sano Mountain.
##         REFNUM PropExp
##      1:      1       3
##      2:      2       3
##      3:      3       3
##      4:      4       3
##      5:      5       3
##     ---               
## 902293: 902293       3
## 902294: 902294       3
## 902295: 902295       3
## 902296: 902296       3
## 902297: 902297       3
```

```r
storm.dt[, PropD:=PROPDMG*(10)^PropExp]
```

```
##         STATE__           BGN_DATE    BGN_TIME TIME_ZONE COUNTY
##      1:       1  4/18/1950 0:00:00        0130       CST     97
##      2:       1  4/18/1950 0:00:00        0145       CST      3
##      3:       1  2/20/1951 0:00:00        1600       CST     57
##      4:       1   6/8/1951 0:00:00        0900       CST     89
##      5:       1 11/15/1951 0:00:00        1500       CST     43
##     ---                                                        
## 902293:      56 11/30/2011 0:00:00 10:30:00 PM       MST      7
## 902294:      30 11/10/2011 0:00:00 02:48:00 PM       MST      9
## 902295:       2  11/8/2011 0:00:00 02:58:00 PM       AKS    213
## 902296:       2  11/9/2011 0:00:00 10:21:00 AM       AKS    202
## 902297:       1 11/28/2011 0:00:00 08:00:00 PM       CST      6
##           COUNTYNAME STATE     EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI
##      1:       MOBILE    AL    TORNADO         0                   
##      2:      BALDWIN    AL    TORNADO         0                   
##      3:      FAYETTE    AL    TORNADO         0                   
##      4:      MADISON    AL    TORNADO         0                   
##      5:      CULLMAN    AL    TORNADO         0                   
##     ---                                                           
## 902293: WYZ007 - 017    WY  HIGH WIND         0                   
## 902294: MTZ009 - 010    MT  HIGH WIND         0                   
## 902295:       AKZ213    AK  HIGH WIND         0                   
## 902296:       AKZ202    AK   BLIZZARD         0                   
## 902297:       ALZ006    AL HEAVY SNOW         0                   
##                   END_DATE    END_TIME COUNTY_END COUNTYENDN END_RANGE
##      1:                                         0         NA         0
##      2:                                         0         NA         0
##      3:                                         0         NA         0
##      4:                                         0         NA         0
##      5:                                         0         NA         0
##     ---                                                               
## 902293: 11/30/2011 0:00:00 10:30:00 PM          0         NA         0
## 902294: 11/10/2011 0:00:00 02:48:00 PM          0         NA         0
## 902295:  11/9/2011 0:00:00 01:15:00 PM          0         NA         0
## 902296:  11/9/2011 0:00:00 05:00:00 PM          0         NA         0
## 902297: 11/29/2011 0:00:00 04:00:00 AM          0         NA         0
##         END_AZI END_LOCATI LENGTH WIDTH  F MAG FATALITIES INJURIES PROPDMG
##      1:                      14.0   100  3   0          0       15    25.0
##      2:                       2.0   150  2   0          0        0     2.5
##      3:                       0.1   123  2   0          0        2    25.0
##      4:                       0.0   100  2   0          0        2     2.5
##      5:                       0.0   150  2   0          0        2     2.5
##     ---                                                                   
## 902293:                       0.0     0 NA  66          0        0     0.0
## 902294:                       0.0     0 NA  52          0        0     0.0
## 902295:                       0.0     0 NA  81          0        0     0.0
## 902296:                       0.0     0 NA   0          0        0     0.0
## 902297:                       0.0     0 NA   0          0        0     0.0
##         PROPDMGEXP CROPDMG CROPDMGEXP WFO                STATEOFFIC
##      1:          K       0                                         
##      2:          K       0                                         
##      3:          K       0                                         
##      4:          K       0                                         
##      5:          K       0                                         
##     ---                                                            
## 902293:          K       0          K RIW WYOMING, Central and West
## 902294:          K       0          K TFX          MONTANA, Central
## 902295:          K       0          K AFG          ALASKA, Northern
## 902296:          K       0          K AFG          ALASKA, Northern
## 902297:          K       0          K HUN            ALABAMA, North
##                                                                                ZONENAMES
##      1:                                                                                 
##      2:                                                                                 
##      3:                                                                                 
##      4:                                                                                 
##      5:                                                                                 
##     ---                                                                                 
## 902293: OWL CREEK & BRIDGER MOUNTAINS - OWL CREEK & BRIDGER MOUNTAINS - WIND RIVER BASIN
## 902294:        NORTH ROCKY MOUNTAIN FRONT - NORTH ROCKY MOUNTAIN FRONT - EASTERN GLACIER
## 902295:                    ST LAWRENCE IS. BERING STRAIT - ST LAWRENCE IS. BERING STRAIT
## 902296:                                    NORTHERN ARCTIC COAST - NORTHERN ARCTIC COAST
## 902297:                                                                MADISON - MADISON
##         LATITUDE LONGITUDE LATITUDE_E LONGITUDE_
##      1:     3040      8812       3051       8806
##      2:     3042      8755          0          0
##      3:     3340      8742          0          0
##      4:     3458      8626          0          0
##      5:     3412      8642          0          0
##     ---                                         
## 902293:        0         0          0          0
## 902294:        0         0          0          0
## 902295:        0         0          0          0
## 902296:        0         0          0          0
## 902297:        0         0          0          0
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         REMARKS
##      1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      2:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      3:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      4:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      5:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##     ---                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
## 902293:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           EPISODE NARRATIVE: A strong cold front moved south through north central Wyoming bringing high wind to the Meeteetse area and along the south slopes of the western Owl Creek Range. Wind gusts to 76 mph were recorded at Madden Reservoir.EVENT NARRATIVE: 
## 902294:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      EPISODE NARRATIVE: A strong westerly flow aloft produced gusty winds at the surface along the Rocky Mountain front and over the plains of Central Montana. Wind gusts in excess of 60 mph were reported.EVENT NARRATIVE: A wind gust to 60 mph was reported at East Glacier Park 1ENE (the Two Medicine DOT site).
## 902295: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902296: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902297:                           EPISODE NARRATIVE: An intense upper level low developed on the 28th at the base of a highly amplified upper trough across the Great Lakes and Mississippi Valley.  The upper low closed off over the mid South and tracked northeast across the Tennessee Valley during the morning of the 29th.   A warm conveyor belt of heavy rainfall developed in advance of the low which dumped from around 2 to over 5 inches of rain across the eastern two thirds of north Alabama and middle Tennessee.  The highest rain amounts were recorded in Jackson and DeKalb Counties with 3 to 5 inches.  The rain fell over 24 to 36 hour period, with rainfall remaining light to moderate during most its duration.  The rainfall resulted in minor river flooding along the Little River, Big Wills Creek and Paint Rock.   A landslide occurred on Highway 35 just north of Section in Jackson County.  A driver was trapped in his vehicle, but was rescued unharmed.  Trees, boulders and debris blocked 100 to 250 yards of Highway 35.\n\nThe rain mixed with and changed to snow across north Alabama during the afternoon and  evening hours of the 28th, and lasted into the 29th.  The heaviest bursts of snow occurred in northwest Alabama during the afternoon and evening hours, and in north central and northeast Alabama during the overnight and morning hours.  Since ground temperatures were in the 50s, and air temperatures in valley areas only dropped into the mid 30s, most of the snowfall melted on impact with mostly trace amounts reported in valley locations.  However, above 1500 foot elevation, snow accumulations of 1 to 2 inches were reported.  The heaviest amount was 2.3 inches on Monte Sano Mountain, about 5 miles northeast of Huntsville.EVENT NARRATIVE: Snowfall accumulations of up to 2.3 inches were reported on the higher elevations of eastern Madison County.  A snow accumulation of 1.5 inches was reported 2.7 miles south of Gurley, while 2.3 inches was reported 3 miles east of Huntsville atop Monte Sano Mountain.
##         REFNUM PropExp PropD
##      1:      1       3 25000
##      2:      2       3  2500
##      3:      3       3 25000
##      4:      4       3  2500
##      5:      5       3  2500
##     ---                     
## 902293: 902293       3     0
## 902294: 902294       3     0
## 902295: 902295       3     0
## 902296: 902296       3     0
## 902297: 902297       3     0
```

```r
storm.dt[, CropExp:=cropexps.new]
```

```
##         STATE__           BGN_DATE    BGN_TIME TIME_ZONE COUNTY
##      1:       1  4/18/1950 0:00:00        0130       CST     97
##      2:       1  4/18/1950 0:00:00        0145       CST      3
##      3:       1  2/20/1951 0:00:00        1600       CST     57
##      4:       1   6/8/1951 0:00:00        0900       CST     89
##      5:       1 11/15/1951 0:00:00        1500       CST     43
##     ---                                                        
## 902293:      56 11/30/2011 0:00:00 10:30:00 PM       MST      7
## 902294:      30 11/10/2011 0:00:00 02:48:00 PM       MST      9
## 902295:       2  11/8/2011 0:00:00 02:58:00 PM       AKS    213
## 902296:       2  11/9/2011 0:00:00 10:21:00 AM       AKS    202
## 902297:       1 11/28/2011 0:00:00 08:00:00 PM       CST      6
##           COUNTYNAME STATE     EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI
##      1:       MOBILE    AL    TORNADO         0                   
##      2:      BALDWIN    AL    TORNADO         0                   
##      3:      FAYETTE    AL    TORNADO         0                   
##      4:      MADISON    AL    TORNADO         0                   
##      5:      CULLMAN    AL    TORNADO         0                   
##     ---                                                           
## 902293: WYZ007 - 017    WY  HIGH WIND         0                   
## 902294: MTZ009 - 010    MT  HIGH WIND         0                   
## 902295:       AKZ213    AK  HIGH WIND         0                   
## 902296:       AKZ202    AK   BLIZZARD         0                   
## 902297:       ALZ006    AL HEAVY SNOW         0                   
##                   END_DATE    END_TIME COUNTY_END COUNTYENDN END_RANGE
##      1:                                         0         NA         0
##      2:                                         0         NA         0
##      3:                                         0         NA         0
##      4:                                         0         NA         0
##      5:                                         0         NA         0
##     ---                                                               
## 902293: 11/30/2011 0:00:00 10:30:00 PM          0         NA         0
## 902294: 11/10/2011 0:00:00 02:48:00 PM          0         NA         0
## 902295:  11/9/2011 0:00:00 01:15:00 PM          0         NA         0
## 902296:  11/9/2011 0:00:00 05:00:00 PM          0         NA         0
## 902297: 11/29/2011 0:00:00 04:00:00 AM          0         NA         0
##         END_AZI END_LOCATI LENGTH WIDTH  F MAG FATALITIES INJURIES PROPDMG
##      1:                      14.0   100  3   0          0       15    25.0
##      2:                       2.0   150  2   0          0        0     2.5
##      3:                       0.1   123  2   0          0        2    25.0
##      4:                       0.0   100  2   0          0        2     2.5
##      5:                       0.0   150  2   0          0        2     2.5
##     ---                                                                   
## 902293:                       0.0     0 NA  66          0        0     0.0
## 902294:                       0.0     0 NA  52          0        0     0.0
## 902295:                       0.0     0 NA  81          0        0     0.0
## 902296:                       0.0     0 NA   0          0        0     0.0
## 902297:                       0.0     0 NA   0          0        0     0.0
##         PROPDMGEXP CROPDMG CROPDMGEXP WFO                STATEOFFIC
##      1:          K       0                                         
##      2:          K       0                                         
##      3:          K       0                                         
##      4:          K       0                                         
##      5:          K       0                                         
##     ---                                                            
## 902293:          K       0          K RIW WYOMING, Central and West
## 902294:          K       0          K TFX          MONTANA, Central
## 902295:          K       0          K AFG          ALASKA, Northern
## 902296:          K       0          K AFG          ALASKA, Northern
## 902297:          K       0          K HUN            ALABAMA, North
##                                                                                ZONENAMES
##      1:                                                                                 
##      2:                                                                                 
##      3:                                                                                 
##      4:                                                                                 
##      5:                                                                                 
##     ---                                                                                 
## 902293: OWL CREEK & BRIDGER MOUNTAINS - OWL CREEK & BRIDGER MOUNTAINS - WIND RIVER BASIN
## 902294:        NORTH ROCKY MOUNTAIN FRONT - NORTH ROCKY MOUNTAIN FRONT - EASTERN GLACIER
## 902295:                    ST LAWRENCE IS. BERING STRAIT - ST LAWRENCE IS. BERING STRAIT
## 902296:                                    NORTHERN ARCTIC COAST - NORTHERN ARCTIC COAST
## 902297:                                                                MADISON - MADISON
##         LATITUDE LONGITUDE LATITUDE_E LONGITUDE_
##      1:     3040      8812       3051       8806
##      2:     3042      8755          0          0
##      3:     3340      8742          0          0
##      4:     3458      8626          0          0
##      5:     3412      8642          0          0
##     ---                                         
## 902293:        0         0          0          0
## 902294:        0         0          0          0
## 902295:        0         0          0          0
## 902296:        0         0          0          0
## 902297:        0         0          0          0
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         REMARKS
##      1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      2:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      3:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      4:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      5:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##     ---                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
## 902293:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           EPISODE NARRATIVE: A strong cold front moved south through north central Wyoming bringing high wind to the Meeteetse area and along the south slopes of the western Owl Creek Range. Wind gusts to 76 mph were recorded at Madden Reservoir.EVENT NARRATIVE: 
## 902294:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      EPISODE NARRATIVE: A strong westerly flow aloft produced gusty winds at the surface along the Rocky Mountain front and over the plains of Central Montana. Wind gusts in excess of 60 mph were reported.EVENT NARRATIVE: A wind gust to 60 mph was reported at East Glacier Park 1ENE (the Two Medicine DOT site).
## 902295: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902296: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902297:                           EPISODE NARRATIVE: An intense upper level low developed on the 28th at the base of a highly amplified upper trough across the Great Lakes and Mississippi Valley.  The upper low closed off over the mid South and tracked northeast across the Tennessee Valley during the morning of the 29th.   A warm conveyor belt of heavy rainfall developed in advance of the low which dumped from around 2 to over 5 inches of rain across the eastern two thirds of north Alabama and middle Tennessee.  The highest rain amounts were recorded in Jackson and DeKalb Counties with 3 to 5 inches.  The rain fell over 24 to 36 hour period, with rainfall remaining light to moderate during most its duration.  The rainfall resulted in minor river flooding along the Little River, Big Wills Creek and Paint Rock.   A landslide occurred on Highway 35 just north of Section in Jackson County.  A driver was trapped in his vehicle, but was rescued unharmed.  Trees, boulders and debris blocked 100 to 250 yards of Highway 35.\n\nThe rain mixed with and changed to snow across north Alabama during the afternoon and  evening hours of the 28th, and lasted into the 29th.  The heaviest bursts of snow occurred in northwest Alabama during the afternoon and evening hours, and in north central and northeast Alabama during the overnight and morning hours.  Since ground temperatures were in the 50s, and air temperatures in valley areas only dropped into the mid 30s, most of the snowfall melted on impact with mostly trace amounts reported in valley locations.  However, above 1500 foot elevation, snow accumulations of 1 to 2 inches were reported.  The heaviest amount was 2.3 inches on Monte Sano Mountain, about 5 miles northeast of Huntsville.EVENT NARRATIVE: Snowfall accumulations of up to 2.3 inches were reported on the higher elevations of eastern Madison County.  A snow accumulation of 1.5 inches was reported 2.7 miles south of Gurley, while 2.3 inches was reported 3 miles east of Huntsville atop Monte Sano Mountain.
##         REFNUM PropExp PropD CropExp
##      1:      1       3 25000       0
##      2:      2       3  2500       0
##      3:      3       3 25000       0
##      4:      4       3  2500       0
##      5:      5       3  2500       0
##     ---                             
## 902293: 902293       3     0       3
## 902294: 902294       3     0       3
## 902295: 902295       3     0       3
## 902296: 902296       3     0       3
## 902297: 902297       3     0       3
```

```r
storm.dt[, CropD:=CROPDMG*(10)^CropExp]
```

```
##         STATE__           BGN_DATE    BGN_TIME TIME_ZONE COUNTY
##      1:       1  4/18/1950 0:00:00        0130       CST     97
##      2:       1  4/18/1950 0:00:00        0145       CST      3
##      3:       1  2/20/1951 0:00:00        1600       CST     57
##      4:       1   6/8/1951 0:00:00        0900       CST     89
##      5:       1 11/15/1951 0:00:00        1500       CST     43
##     ---                                                        
## 902293:      56 11/30/2011 0:00:00 10:30:00 PM       MST      7
## 902294:      30 11/10/2011 0:00:00 02:48:00 PM       MST      9
## 902295:       2  11/8/2011 0:00:00 02:58:00 PM       AKS    213
## 902296:       2  11/9/2011 0:00:00 10:21:00 AM       AKS    202
## 902297:       1 11/28/2011 0:00:00 08:00:00 PM       CST      6
##           COUNTYNAME STATE     EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI
##      1:       MOBILE    AL    TORNADO         0                   
##      2:      BALDWIN    AL    TORNADO         0                   
##      3:      FAYETTE    AL    TORNADO         0                   
##      4:      MADISON    AL    TORNADO         0                   
##      5:      CULLMAN    AL    TORNADO         0                   
##     ---                                                           
## 902293: WYZ007 - 017    WY  HIGH WIND         0                   
## 902294: MTZ009 - 010    MT  HIGH WIND         0                   
## 902295:       AKZ213    AK  HIGH WIND         0                   
## 902296:       AKZ202    AK   BLIZZARD         0                   
## 902297:       ALZ006    AL HEAVY SNOW         0                   
##                   END_DATE    END_TIME COUNTY_END COUNTYENDN END_RANGE
##      1:                                         0         NA         0
##      2:                                         0         NA         0
##      3:                                         0         NA         0
##      4:                                         0         NA         0
##      5:                                         0         NA         0
##     ---                                                               
## 902293: 11/30/2011 0:00:00 10:30:00 PM          0         NA         0
## 902294: 11/10/2011 0:00:00 02:48:00 PM          0         NA         0
## 902295:  11/9/2011 0:00:00 01:15:00 PM          0         NA         0
## 902296:  11/9/2011 0:00:00 05:00:00 PM          0         NA         0
## 902297: 11/29/2011 0:00:00 04:00:00 AM          0         NA         0
##         END_AZI END_LOCATI LENGTH WIDTH  F MAG FATALITIES INJURIES PROPDMG
##      1:                      14.0   100  3   0          0       15    25.0
##      2:                       2.0   150  2   0          0        0     2.5
##      3:                       0.1   123  2   0          0        2    25.0
##      4:                       0.0   100  2   0          0        2     2.5
##      5:                       0.0   150  2   0          0        2     2.5
##     ---                                                                   
## 902293:                       0.0     0 NA  66          0        0     0.0
## 902294:                       0.0     0 NA  52          0        0     0.0
## 902295:                       0.0     0 NA  81          0        0     0.0
## 902296:                       0.0     0 NA   0          0        0     0.0
## 902297:                       0.0     0 NA   0          0        0     0.0
##         PROPDMGEXP CROPDMG CROPDMGEXP WFO                STATEOFFIC
##      1:          K       0                                         
##      2:          K       0                                         
##      3:          K       0                                         
##      4:          K       0                                         
##      5:          K       0                                         
##     ---                                                            
## 902293:          K       0          K RIW WYOMING, Central and West
## 902294:          K       0          K TFX          MONTANA, Central
## 902295:          K       0          K AFG          ALASKA, Northern
## 902296:          K       0          K AFG          ALASKA, Northern
## 902297:          K       0          K HUN            ALABAMA, North
##                                                                                ZONENAMES
##      1:                                                                                 
##      2:                                                                                 
##      3:                                                                                 
##      4:                                                                                 
##      5:                                                                                 
##     ---                                                                                 
## 902293: OWL CREEK & BRIDGER MOUNTAINS - OWL CREEK & BRIDGER MOUNTAINS - WIND RIVER BASIN
## 902294:        NORTH ROCKY MOUNTAIN FRONT - NORTH ROCKY MOUNTAIN FRONT - EASTERN GLACIER
## 902295:                    ST LAWRENCE IS. BERING STRAIT - ST LAWRENCE IS. BERING STRAIT
## 902296:                                    NORTHERN ARCTIC COAST - NORTHERN ARCTIC COAST
## 902297:                                                                MADISON - MADISON
##         LATITUDE LONGITUDE LATITUDE_E LONGITUDE_
##      1:     3040      8812       3051       8806
##      2:     3042      8755          0          0
##      3:     3340      8742          0          0
##      4:     3458      8626          0          0
##      5:     3412      8642          0          0
##     ---                                         
## 902293:        0         0          0          0
## 902294:        0         0          0          0
## 902295:        0         0          0          0
## 902296:        0         0          0          0
## 902297:        0         0          0          0
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         REMARKS
##      1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      2:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      3:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      4:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##      5:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
##     ---                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
## 902293:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           EPISODE NARRATIVE: A strong cold front moved south through north central Wyoming bringing high wind to the Meeteetse area and along the south slopes of the western Owl Creek Range. Wind gusts to 76 mph were recorded at Madden Reservoir.EVENT NARRATIVE: 
## 902294:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      EPISODE NARRATIVE: A strong westerly flow aloft produced gusty winds at the surface along the Rocky Mountain front and over the plains of Central Montana. Wind gusts in excess of 60 mph were reported.EVENT NARRATIVE: A wind gust to 60 mph was reported at East Glacier Park 1ENE (the Two Medicine DOT site).
## 902295: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902296: EPISODE NARRATIVE: A 960 mb low over the southern Aleutians at 0300AKST on the 8th intensified to 945 mb near the Gulf of Anadyr by 2100AKST on the 8th. The low crossed the Chukotsk Peninsula as a 956 mb low at 0900AKST on the 9th, and moved into the southern Chukchi Sea as a 958 mb low by 2100AKST on the 9th. The low then tracked to the northwest and weakened to 975 mb about 150 miles north of Wrangel Island by 1500AKST on the 10th. The storm was one of the strongest storms to impact the west coast of Alaska since November 1974. \n\nZone 201: Blizzard conditions were observed at Wainwright from approximately 1153AKST through 1611AKST on the 9th. The visibility was frequently reduced to one quarter mile in snow and blowing snow. There was a peak wind gust to 43kt (50 mph) at the Wainwright ASOS. During this event, there was also a peak wind gust to \n68 kt (78 mph) at the Cape Lisburne AWOS. \n\nZone 202: Blizzard conditions were observed at Barrow from approximately 1021AKST through 1700AKST on the 9th. The visibility was frequently reduced to one quarter mile or less in blowing snow. There was a peak wind gust to 46 kt (53 mph) at the Barrow ASOS. \n\nZone 207: Blizzard conditions were observed at Kivalina from approximately 0400AKST through 1230AKST on the 9th. The visibility was frequently reduced to one quarter of a mile in snow and blowing snow. There was a peak wind gust to 61 kt (70 mph) at the Kivalina ASOS.  The doors to the village transportation shed were blown out to sea.  Many homes lost portions of their tin roofing, and satellite dishes were ripped off of roofs. One home had its door blown off.  At Point Hope, severe blizzard conditions were observed. There was a peak wind gust of 68 kt (78 mph) at the Point Hope AWOS before power was lost to the AWOS. It was estimated that the wind gusted as high as 85 mph in the village during the height of the storm during the morning and early afternoon hours on the 9th. Five power poles were knocked down in the storm EVENT NARRATIVE: 
## 902297:                           EPISODE NARRATIVE: An intense upper level low developed on the 28th at the base of a highly amplified upper trough across the Great Lakes and Mississippi Valley.  The upper low closed off over the mid South and tracked northeast across the Tennessee Valley during the morning of the 29th.   A warm conveyor belt of heavy rainfall developed in advance of the low which dumped from around 2 to over 5 inches of rain across the eastern two thirds of north Alabama and middle Tennessee.  The highest rain amounts were recorded in Jackson and DeKalb Counties with 3 to 5 inches.  The rain fell over 24 to 36 hour period, with rainfall remaining light to moderate during most its duration.  The rainfall resulted in minor river flooding along the Little River, Big Wills Creek and Paint Rock.   A landslide occurred on Highway 35 just north of Section in Jackson County.  A driver was trapped in his vehicle, but was rescued unharmed.  Trees, boulders and debris blocked 100 to 250 yards of Highway 35.\n\nThe rain mixed with and changed to snow across north Alabama during the afternoon and  evening hours of the 28th, and lasted into the 29th.  The heaviest bursts of snow occurred in northwest Alabama during the afternoon and evening hours, and in north central and northeast Alabama during the overnight and morning hours.  Since ground temperatures were in the 50s, and air temperatures in valley areas only dropped into the mid 30s, most of the snowfall melted on impact with mostly trace amounts reported in valley locations.  However, above 1500 foot elevation, snow accumulations of 1 to 2 inches were reported.  The heaviest amount was 2.3 inches on Monte Sano Mountain, about 5 miles northeast of Huntsville.EVENT NARRATIVE: Snowfall accumulations of up to 2.3 inches were reported on the higher elevations of eastern Madison County.  A snow accumulation of 1.5 inches was reported 2.7 miles south of Gurley, while 2.3 inches was reported 3 miles east of Huntsville atop Monte Sano Mountain.
##         REFNUM PropExp PropD CropExp CropD
##      1:      1       3 25000       0     0
##      2:      2       3  2500       0     0
##      3:      3       3 25000       0     0
##      4:      4       3  2500       0     0
##      5:      5       3  2500       0     0
##     ---                                   
## 902293: 902293       3     0       3     0
## 902294: 902294       3     0       3     0
## 902295: 902295       3     0       3     0
## 902296: 902296       3     0       3     0
## 902297: 902297       3     0       3     0
```

```r
prop.dmg<-strom.col.ordered("PropD")
cnum <- dim(prop.dmg)[1]
sum(prop.dmg[21:cnum]$V1)
```

```
## [1] 12844214821
```

```r
sum(prop.dmg[1:cnum]$V1)
```

```
## [1] 428224866338
```

```r
prop.dmg[1:20]
```

```
##                        EVTYPE           V1
##  1:                     FLOOD 144657709807
##  2:         HURRICANE/TYPHOON  69305840000
##  3:                   TORNADO  56947380676
##  4:               STORM SURGE  43323536000
##  5:               FLASH FLOOD  16822673978
##  6:                      HAIL  15735267063
##  7:                 HURRICANE  11868319010
##  8:            TROPICAL STORM   7703890550
##  9:              WINTER STORM   6688497251
## 10:                 HIGH WIND   5270046295
## 11:               RIVER FLOOD   5118945500
## 12:                  WILDFIRE   4765114000
## 13:          STORM SURGE/TIDE   4641188000
## 14:                 TSTM WIND   4484928495
## 15:                 ICE STORM   3944927860
## 16:         THUNDERSTORM WIND   3483122472
## 17:            HURRICANE OPAL   3172846000
## 18:          WILD/FOREST FIRE   3001829500
## 19: HEAVY RAIN/SEVERE WEATHER   2500000000
## 20:        THUNDERSTORM WINDS   1944589059
```

```r
crop.dmg<-strom.col.ordered("CropD")
cnum <- dim(crop.dmg)[1]
sum(crop.dmg[21:cnum]$V1)
```

```
## [1] 2164289788
```

```r
sum(crop.dmg[1:cnum]$V1)
```

```
## [1] 49104192181
```

```r
crop.dmg[1:20]
```

```
##                EVTYPE          V1
##  1:           DROUGHT 13972566000
##  2:             FLOOD  5661968450
##  3:       RIVER FLOOD  5029459000
##  4:         ICE STORM  5022113500
##  5:              HAIL  3025954473
##  6:         HURRICANE  2741910000
##  7: HURRICANE/TYPHOON  2607872800
##  8:       FLASH FLOOD  1421317100
##  9:      EXTREME COLD  1292973000
## 10:      FROST/FREEZE  1094086000
## 11:        HEAVY RAIN   733399800
## 12:    TROPICAL STORM   678346000
## 13:         HIGH WIND   638571300
## 14:         TSTM WIND   554007350
## 15:    EXCESSIVE HEAT   492402000
## 16:            FREEZE   446225000
## 17:           TORNADO   414953270
## 18: THUNDERSTORM WIND   414843050
## 19:              HEAT   401461500
## 20:          WILDFIRE   295472800
```
