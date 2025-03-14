Health and Economic Impacts of Weather Events: Analyzing NOAA Data
================
C Salafia
2025-03-14

# Introduction

Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and
Atmospheric Administration’s (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage.

# Data

The data for this project [can be downloaded as a bz file
here.](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

# Data Processing

This section contains the code for reading, cleaning, and organizing the
data. The only columns used in this analysis are:

- EVTYPE - Event Type
- FATALITIES - Number of fatalities associated with an event
- INJURIES - Number of injuries associated with an event
- PROPDMG - Property damage in USD
- PROPDMGEXP - Exponent for property damage (K, M, B)
- CROPDMG - Crop damage in USD
- CROPDMGEXP - Exponent for crop damage (K, M, B)
- BGN_DATE - Beginning date of the event
- END_DATE - End date of the event
- STATE - State where the event occurred

# Loading the required libraries

``` r
library(data.table)  # For efficient data manipulation
library(ggplot2)     # For data visualization
library(reshape2)    # For reshaping data (melt function)
library(knitr)       # For RPubs compatibility
library(rmarkdown)   # For generating reports
library(xtable)
```

# Reading and exploring the data

``` r
rawData <- fread("stormData.csv")
```

## Column names

``` r
names(rawData)
```

    ##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
    ## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
    ## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
    ## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_" "REMARKS"    "REFNUM"

## Structure

``` r
str(rawData)
```

    ## Classes 'data.table' and 'data.frame':   902297 obs. of  37 variables:
    ##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
    ##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
    ##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
    ##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
    ##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
    ##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
    ##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
    ##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ BGN_AZI   : chr  "" "" "" "" ...
    ##  $ BGN_LOCATI: chr  "" "" "" "" ...
    ##  $ END_DATE  : chr  "" "" "" "" ...
    ##  $ END_TIME  : chr  "" "" "" "" ...
    ##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
    ##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ END_AZI   : chr  "" "" "" "" ...
    ##  $ END_LOCATI: chr  "" "" "" "" ...
    ##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
    ##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
    ##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
    ##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
    ##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
    ##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
    ##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CROPDMGEXP: chr  "" "" "" "" ...
    ##  $ WFO       : chr  "" "" "" "" ...
    ##  $ STATEOFFIC: chr  "" "" "" "" ...
    ##  $ ZONENAMES : chr  "" "" "" "" ...
    ##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
    ##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
    ##  $ LATITUDE_E: num  3051 0 0 0 0 ...
    ##  $ LONGITUDE_: num  8806 0 0 0 0 ...
    ##  $ REMARKS   : chr  "" "" "" "" ...
    ##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

## Head

``` r
head(rawData)
```

    ##    STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME  STATE  EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
    ##      <num>             <char>   <char>    <char>  <num>     <char> <char>  <char>     <num>  <char>     <char>   <char>   <char>      <num>
    ## 1:       1  4/18/1950 0:00:00     0130       CST     97     MOBILE     AL TORNADO         0                                               0
    ## 2:       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN     AL TORNADO         0                                               0
    ## 3:       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE     AL TORNADO         0                                               0
    ## 4:       1   6/8/1951 0:00:00     0900       CST     89    MADISON     AL TORNADO         0                                               0
    ## 5:       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN     AL TORNADO         0                                               0
    ## 6:       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE     AL TORNADO         0                                               0
    ##    COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH     F   MAG FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP    WFO
    ##        <lgcl>     <num>  <char>     <char>  <num> <num> <int> <num>      <num>    <num>   <num>     <char>   <num>     <char> <char>
    ## 1:         NA         0                      14.0   100     3     0          0       15    25.0          K       0                  
    ## 2:         NA         0                       2.0   150     2     0          0        0     2.5          K       0                  
    ## 3:         NA         0                       0.1   123     2     0          0        2    25.0          K       0                  
    ## 4:         NA         0                       0.0   100     2     0          0        2     2.5          K       0                  
    ## 5:         NA         0                       0.0   150     2     0          0        2     2.5          K       0                  
    ## 6:         NA         0                       1.5   177     2     0          0        6     2.5          K       0                  
    ##    STATEOFFIC ZONENAMES LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
    ##        <char>    <char>    <num>     <num>      <num>      <num>  <char>  <num>
    ## 1:                          3040      8812       3051       8806              1
    ## 2:                          3042      8755          0          0              2
    ## 3:                          3340      8742          0          0              3
    ## 4:                          3458      8626          0          0              4
    ## 5:                          3412      8642          0          0              5
    ## 6:                          3450      8748          0          0              6

# Data Processing

## Subsetting the data

``` r
stormData <- subset(rawData, EVTYPE != "?" &
                         (FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0),
                       select = c("EVTYPE",
                                  "FATALITIES",
                                  "INJURIES", 
                                  "PROPDMG",
                                  "PROPDMGEXP",
                                  "CROPDMG",
                                  "CROPDMGEXP",
                                  "BGN_DATE",
                                  "END_DATE",
                                  "STATE"))
dim(stormData)
```

    ## [1] 254632     10

## Cleaning the Event Type data

``` r
length(unique(stormData$EVTYPE))
```

    ## [1] 487

To better analyze the data, the event type (column EVTYPE) names are
converted to upper case and combined into categories. Some entries
contained incorrect spelling, plurals, and mixed cases.

``` r
stormData$EVTYPE <- toupper(stormData$EVTYPE)

# AVALANCHE
stormData$EVTYPE <- gsub('.*AVALANCE.*', 'AVALANCHE', stormData$EVTYPE)

# BLIZZARD
stormData$EVTYPE <- gsub('.*BLIZZARD.*', 'BLIZZARD', stormData$EVTYPE)

# CLOUD
stormData$EVTYPE <- gsub('.*CLOUD.*', 'CLOUD', stormData$EVTYPE)

# COLD
stormData$EVTYPE <- gsub('.*COLD.*', 'COLD', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*FREEZ.*', 'COLD', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*FROST.*', 'COLD', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*ICE.*', 'COLD', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*LOW TEMPERATURE RECORD.*', 'COLD', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*LO.*TEMP.*', 'COLD', stormData$EVTYPE)

# DRY
stormData$EVTYPE <- gsub('.*DRY.*', 'DRY', stormData$EVTYPE)

# DUST
stormData$EVTYPE <- gsub('.*DUST.*', 'DUST', stormData$EVTYPE)

# FIRE
stormData$EVTYPE <- gsub('.*FIRE.*', 'FIRE', stormData$EVTYPE)

# FLOOD
stormData$EVTYPE <- gsub('.*FLOOD.*', 'FLOOD', stormData$EVTYPE)

# FOG
stormData$EVTYPE <- gsub('.*FOG.*', 'FOG', stormData$EVTYPE)

# HAIL
stormData$EVTYPE <- gsub('.*HAIL.*', 'HAIL', stormData$EVTYPE)

# HEAT
stormData$EVTYPE <- gsub('.*HEAT.*', 'HEAT', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*WARM.*', 'HEAT', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*HIGH.*TEMP.*', 'HEAT', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*RECORD HIGH TEMPERATURES.*', 'HEAT', stormData$EVTYPE)

# HYPOTHERMIA/EXPOSURE
stormData$EVTYPE <- gsub('.*HYPOTHERMIA.*', 'HYPOTHERMIA/EXPOSURE', stormData$EVTYPE)

# LANDSLIDE
stormData$EVTYPE <- gsub('.*LANDSLIDE.*', 'LANDSLIDE', stormData$EVTYPE)

# LIGHTNING
stormData$EVTYPE <- gsub('^LIGHTNING.*', 'LIGHTNING', stormData$EVTYPE)
stormData$EVTYPE <- gsub('^LIGNTNING.*', 'LIGHTNING', stormData$EVTYPE)
stormData$EVTYPE <- gsub('^LIGHTING.*', 'LIGHTNING', stormData$EVTYPE)

# MICROBURST
stormData$EVTYPE <- gsub('.*MICROBURST.*', 'MICROBURST', stormData$EVTYPE)

# MUDSLIDE
stormData$EVTYPE <- gsub('.*MUDSLIDE.*', 'MUDSLIDE', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*MUD SLIDE.*', 'MUDSLIDE', stormData$EVTYPE)

# RAIN
stormData$EVTYPE <- gsub('.*RAIN.*', 'RAIN', stormData$EVTYPE)

# RIP CURRENT
stormData$EVTYPE <- gsub('.*RIP CURRENT.*', 'RIP CURRENT', stormData$EVTYPE)

# STORM
stormData$EVTYPE <- gsub('.*STORM.*', 'STORM', stormData$EVTYPE)

# SUMMARY
stormData$EVTYPE <- gsub('.*SUMMARY.*', 'SUMMARY', stormData$EVTYPE)

# TORNADO
stormData$EVTYPE <- gsub('.*TORNADO.*', 'TORNADO', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*TORNDAO.*', 'TORNADO', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*LANDSPOUT.*', 'TORNADO', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*WATERSPOUT.*', 'TORNADO', stormData$EVTYPE)

# SURF
stormData$EVTYPE <- gsub('.*SURF.*', 'SURF', stormData$EVTYPE)

# VOLCANIC
stormData$EVTYPE <- gsub('.*VOLCANIC.*', 'VOLCANIC', stormData$EVTYPE)

# WET
stormData$EVTYPE <- gsub('.*WET.*', 'WET', stormData$EVTYPE)

# WIND
stormData$EVTYPE <- gsub('.*WIND.*', 'WIND', stormData$EVTYPE)

# WINTER
stormData$EVTYPE <- gsub('.*WINTER.*', 'WINTER', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*WINTRY.*', 'WINTER', stormData$EVTYPE)
stormData$EVTYPE <- gsub('.*SNOW.*', 'WINTER', stormData$EVTYPE)
```

This step reduced the number of unique events to 81.

``` r
length(unique(stormData$EVTYPE))
```

    ## [1] 81

## Cleaning Date data

The date data is stored as factors. This step converted them to **as
date** type. This step creates 4 new variables:

- DATE_START - Beginning date of the event
- DATE_END - Ending date of the event
- YEAR - Year the event occurred
- DURATION - Duration (in hours) of the event

``` r
stormData$DATE_START <- as.Date(stormData$BGN_DATE, format = "%m/%d/%Y")
stormData$DATE_END <- as.Date(stormData$END_DATE, format = "%m/%d/%Y")
stormData$YEAR <- as.integer(format(stormData$DATE_START, "%Y"))
stormData$DURATION <- as.numeric(stormData$DATE_END - stormData$DATE_START)/3600
```

## Cleaning Economic Data

To calculate costs, the PROPDMGEXP and CROPDMGEXP variables should be
mapped to a factor which will be used to calculate the costs for both
property and crop damage. Two new variables should be created to store
damage costs:

- PROP_COST
- CROP_COST

``` r
# function to get factor
getMultiplier <- function(exp) {
    exp <- toupper(exp);
    if (exp == "")  return (10^0);
    if (exp == "-") return (10^0);
    if (exp == "?") return (10^0);
    if (exp == "+") return (10^0);
    if (exp == "0") return (10^0);
    if (exp == "1") return (10^1);
    if (exp == "2") return (10^2);
    if (exp == "3") return (10^3);
    if (exp == "4") return (10^4);
    if (exp == "5") return (10^5);
    if (exp == "6") return (10^6);
    if (exp == "7") return (10^7);
    if (exp == "8") return (10^8);
    if (exp == "9") return (10^9);
    if (exp == "H") return (10^2);
    if (exp == "K") return (10^3);
    if (exp == "M") return (10^6);
    if (exp == "B") return (10^9);
    return (NA);
}

# calculate property damage and crop damage costs (in billions)
stormData$PROP_COST <- with(stormData, as.numeric(PROPDMG) * sapply(PROPDMGEXP, getMultiplier))/10^9
stormData$CROP_COST <- with(stormData, as.numeric(CROPDMG) * sapply(CROPDMGEXP, getMultiplier))/10^9
```

## Summarizing the data

This step creates a summarized data set of the health impacts and
economic impacts. Values are sorted in descending order.

``` r
# Health Impact Data
healthImpactData <- aggregate(x = list(HEALTH_IMPACT = stormData$FATALITIES + stormData$INJURIES), 
                                  by = list(EVENT_TYPE = stormData$EVTYPE), 
                                  FUN = sum,
                                  na.rm = TRUE)
healthImpactData <- healthImpactData[order(healthImpactData$HEALTH_IMPACT, decreasing = TRUE),]

# Economic Impact Data
damageCostImpactData <- aggregate(x = list(DAMAGE_IMPACT = stormData$PROP_COST + stormData$CROP_COST), 
                                  by = list(EVENT_TYPE = stormData$EVTYPE), 
                                  FUN = sum,
                                  na.rm = TRUE)
damageCostImpactData <- damageCostImpactData[order(damageCostImpactData$DAMAGE_IMPACT, decreasing = TRUE),]
```

# Results

## Question 1 - Which events are most harmful to population health?

``` r
healthImpactChart <- ggplot(head(healthImpactData, 10),
                            aes(x = reorder(EVENT_TYPE, HEALTH_IMPACT), y = HEALTH_IMPACT, fill = EVENT_TYPE)) +
                            coord_flip() +
                            geom_bar(stat = "identity") + 
                            xlab("Event Type") +
                            ylab("Total Fatalities and Injures") +
                            theme(plot.title = element_text(size = 14, hjust = 0.5)) +
                            ggtitle("Top 10 Most Harmful Weather Events to Population Health")
print(healthImpactChart)
```

![](RR_Project2_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

## Question 2 - Which events have the greatest economic consequenses?

``` r
damageCostImpactChart <- ggplot(head(damageCostImpactData, 10),
                            aes(x = reorder(EVENT_TYPE, DAMAGE_IMPACT), y = DAMAGE_IMPACT, fill = EVENT_TYPE)) +
                            coord_flip() +
                            geom_bar(stat = "identity") + 
                            xlab("Event Type") +
                            ylab("Total Property / Crop Damage Cost (in Billions)") +
                            theme(plot.title = element_text(size = 14, hjust = 0.5)) +
                            ggtitle("Top 10 Events with Greatest Economic Consequences")
print(damageCostImpactChart)
```

![](RR_Project2_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

# Conclusion

The analysis of the noaa Storm Data shows the following:

- Tornadoes are responsible for the greatest number of injuries and
  fatalities.
- Floods are responsible for the greatest economic costs in terms of
  property and crop damage.
