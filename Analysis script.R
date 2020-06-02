library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)

# Get and read the data

dataFileName <- "repdata-data-StormData.csv.bz2"
if(download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              dataFileName)) stop("Error downloading file")
if(!file.exists(dataFileName)) stop("Could not find data file")

# read.csv can read compressed files
stormData<-as_tibble(read.csv(dataFileName))

str(stormData)
dim(stormData)
summary(stormData)

# Keep only what we need
stormData <- stormData %>% select(EVTYPE,FATALITIES,INJURIES,PROPDMG,
                                  PROPDMGEXP,CROPDMG,CROPDMGEXP)

# Cleanup EVTYPE
stormData <- stormData %>% mutate(EVTYPE = as.factor(toupper(str_squish(EVTYPE))))


# Harm to population health is captured in FATALITIES and INJURIES

sumHarmSD <- stormData %>% 
        mutate(harm = FATALITIES + INJURIES) %>%                # calculate harm
        group_by(EVTYPE) %>%                                    # group events
        summarize(catHarm = sum(harm)) %>%                      # summarize harm by event
        mutate(totalHarm = sum(catHarm),                        # calculate total harm
               percentHarm = catHarm/totalHarm*100) %>%         # calculate percentages
        arrange(desc(percentHarm))                              # sort descending


topHarmSD <- sumHarmSD %>% slice(n=1)                           # select top 1

# Create "Other" row
otherTotalHarm = topHarmSD$totalHarm[1]
otherCatHarm = otherTotalHarm - sum(topHarmSD$catHarm)
otherPercentHarm = 100 - sum(topHarmSD$percentHarm)

topHarmSD <- topHarmSD %>%
        add_row(EVTYPE="All Other Events",
                catHarm=otherCatHarm,
                totalHarm=otherTotalHarm,
                percentHarm=otherPercentHarm)

# Events excluding top contributor

highHarmSD <- sumHarmSD %>% slice(n=2:11)                       # select next top 10
highHarmSD$EVTYPE <- factor(highHarmSD$EVTYPE, 
                               levels = highHarmSD$EVTYPE)      # sort labels

# Plots

# Pie Chart top contributor
plot1 = ggplot(topHarmSD,
       aes(x="", y=percentHarm, fill=EVTYPE)) +
        geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
        coord_polar("y", start=0) +
        geom_text(aes(label = paste0(round(percentHarm), "%")), 
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL, y = NULL, fill = NULL, 
             title = "Tornadoes vs. Other Events") +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_brewer(palette="Paired") +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666"),
              legend.text = element_text(face = "bold", size = 10),
              legend.position = "bottom")


# Bar chart - 10 other contributors
plot2 = ggplot(highHarmSD,
       aes(x=EVTYPE, y=percentHarm, fill=EVTYPE)) +
        geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
        labs(x = "", y = "Harm (%)", fill = "Event Type", 
             title = "Events excluding Tornadoes") +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_brewer(palette="Paired") +
        theme_classic() +
        theme(axis.line.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666"),
              legend.text = element_text(face = "bold", size = 10))

# Render both in one side-by-side chart
gridExtra::grid.arrange(plot1,plot2,ncol=2, 
                        top = "NOAA Storm Database, 1950 - November 2011:\nHarmful Effects to Population Health")

############

# Other CROPDMGEXP, PROPDMGEXP entries will be ignored
knownMult <- data.frame(exp= c("",  "H",  "h",  "K",  "k",  "M",  "m",  "B",  "b",  "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9"),
                        mult=c( 1,  100,  100,  1e3,  1e3,  1e6,  1e6,  1e9,  1e9,    1,   10,  100,  1e3,  1e4,  1e5,  1e6,  1e7, 1e8,  1e9))

# Count how many rows have uninterpretable exponents

print(unknown <- mean(!(stormData$CROPDMGEXP %in% knownMult$exp & 
                                        stormData$PROPDMGEXP %in% knownMult$exp)))*100

#Eliminate unknown EXP, lookup multipliers (using left_join), calculate total damage

dmgSD <- stormData %>% 
        filter(CROPDMGEXP %in% knownMult$exp,PROPDMGEXP %in% knownMult$exp) %>% 
        left_join(knownMult,by=c("PROPDMGEXP"="exp")) %>% 
        rename(propMult=mult) %>%                               # property multipliers
        left_join(knownMult,by=c("CROPDMGEXP"="exp")) %>% 
        rename(cropMult=mult) %>%                               # crop multipliers
        mutate(damage = PROPDMG*propMult + 
                       CROPDMG*cropMult) %>%                    # calculate damage
        group_by(EVTYPE) %>%                                    # group events
        summarize(catDamage = sum(damage)/10^9) %>%             # summarize harm by event
        mutate(totalDamage = sum(catDamage),                    # calculate total harm
               percentDamage = catDamage/totalDamage*100) %>%   # calculate percentages
        arrange(desc(percentDamage)) %>%                        # sort descending
        slice(n=1:10)
        

dmgSD$EVTYPE <- factor(dmgSD$EVTYPE, 
                            levels = dmgSD$EVTYPE)              # sort labels

ggplot(dmgSD, aes(x=EVTYPE, y=catDamage, fill=EVTYPE)) +
        geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
        labs(x = "", y = "Harm (Billions USD)", fill = "Event Type", 
             title = "NOAA Storm Database, 1950 - November 2011:\nEconomic Consequences") +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_brewer(palette="Paired") +
        theme_classic() +
        theme(axis.line.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666"),
              legend.text = element_text(face = "bold", size = 10))


                              
