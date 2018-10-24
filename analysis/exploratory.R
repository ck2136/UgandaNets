##############################################
# Exploratory Analysis
# Created by: CK
# Created on: 10/19/2018
# Modified by: CK
# Modified on: 10/19/2018
##############################################

# - - - - - - - - - - - - - - - - - - - - - #
## 1. Loading Libraries and Data ----
# - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
setwd("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process")
df <- readRDS("final_hh_df.RDS")
# df <- readRDS("final_hh_df.RDS") # this is for when we have enough info on the survey 4 timepoint


# - - - - - - - - - - - - - - - - - - - - - #
## 2. Plots
# - - - - - - - - - - - - - - - - - - - - - #
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(gridExtra)


#-- use magoro county only and select only the necessary variables
f <- function(x) x-273.15
magoro_df  <- df %>%
    filter(county == 2) %>%
    select(household, stime, netCov, rdtRate, micRate, alt_srtm,
           contains("EVI"), contains("tempK")) %>%
    mutate_at(vars(matches("tempK")),  funs(. - 273.15)) %>%
    mutate_at(vars(matches("EVI")), scale) 



magoro_df %>% colnames
magoro_df %>% head


#-- create plotdf
plotdf <- magoro_df %>%
    gather(EVIlag, EVIvalue, -household, -stime, -netCov, -rdtRate, -micRate, -alt_srtm, -contains("tempK")) %>%
    gather(tempKlag, tempKvalue, -household, -stime, -netCov, -rdtRate, -micRate, -alt_srtm, -contains("EVI")) 

summary(magoro_df)
summary(plotdf)
plotdf %>% head

lapply(plotdf , function(x) { class(x) })

plotdf$EVIlag <- factor(plotdf$EVIlag, levels = c("EVI_0","EVI_16","EVI_32","EVI_48","EVI_64","EVI_80","EVI_96","EVI_112","EVI_128"))
plotdf$tempKlag <- factor(plotdf$tempKlag, levels = c("tempK_0","tempK_16","tempK_32","tempK_48","tempK_64","tempK_80","tempK_96","tempK_112","tempK_128"))

ggplot(data=plotdf, aes(x = EVIvalue, y = rdtRate, colour=EVIlag)) +
    geom_point() + 
    geom_smooth(method = 'gam',
                formula= y ~ s(x, bs="cs", k=3))

summary(magoro)


#-- temperature effect
rdt_temp <- ggplot(data=plotdf, aes(x = tempKvalue, y = rdtRate, colour=tempKlag)) +
    geom_point() + 
    geom_smooth(method = 'gam',
                formula= y ~ s(x, bs="cs", k=3)) +
xlab("Temperature in Celsius") +
    ylab("RDT rate") + theme(legend.position="bottom")


mic_temp <- ggplot(data=plotdf, aes(x = tempKvalue, y = micRate, colour=tempKlag)) +
    geom_point() + 
    geom_smooth(method = 'gam',
                formula= y ~ s(x, bs="cs", k=3)) +
xlab("Temperature in Celsius") +
    ylab("MIC rate") +
    theme_bw()  + theme(legend.position = "bottom")


plot_grid(rdt_temp, mic_temp, labels= "AUTO")

#-- EVI effect

rdt_EVI <- ggplot(data=plotdf, aes(x = EVIvalue, y = rdtRate, colour=tempKlag)) +
    geom_point() + 
    geom_smooth(method = 'gam',
                formula= y ~ s(x, bs="cs", k=3)) +
xlab("EVI") +
    ylab("RDT rate") + theme(legend.position="bottom")


mic_EVI <- ggplot(data=plotdf, aes(x = EVIvalue, y = micRate, colour=tempKlag)) +
    geom_point() + 
    geom_smooth(method = 'gam',
                formula= y ~ s(x, bs="cs", k=3)) +
xlab("EVI") +
    ylab("MIC rate") +
    theme_bw()  + theme(legend.position = "bottom")


plot_grid(rdt_EVI, mic_EVI, labels= "AUTO")
# - - - - - - - - - - - - - - - - - - - - - #
## Figure 1. ggmap
# - - - - - - - - - - - - - - - - - - - - - #
library(sp)
library(rgdal)
library(dplyr)


map <- readRDS("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/map.RDS") # in the process folder


spdf <- SpatialPointsDataFrame(coords = df[,c("longitude","latitude")], data = subset(df, select = 1:ncol(df)), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

uganda_shp <- readOGR(dsn = "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/UGA_adm_shp", layer = "UGA_adm4")

### Here is another shp file from the people at IDS 

uganda_shpa <- readOGR(dsn = "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/PIL_shp", layer = "pilgrim_villages")

subset <- uganda_shpa[uganda_shpa$subcounty == 2,] # we want subcounty 2


temp <- over(spdf, subset) # here we're getting those observations from the spdf points dataframe that intersects with the subset polygon dataframe
temp <- cbind(as.data.frame(df), temp[3]) # merge in the village column
poly1 <- subset # create new subset so that we can join 
#poly1@data <- left_join(poly1@data, aggregate(rdtRate ~ village, FUN = mean, na.rm=TRUE, data = temp)) # here we are aggregating across all survey time points
#poly1@data <- left_join(poly1@data, aggregate(micRate ~ village, FUN = mean, na.rm=TRUE, data = temp)) # here we are aggregating across all survey time points
#poly1@data <- left_join(poly1@data, aggregate(netCov ~ village, FUN = mean, na.rm=TRUE, data = temp)) # same thing for the net coverage

#### Plot RDT+, MIC+, by survey times...
md_time <- temp %>%
  mutate(mi = ifelse(is.na(num_hh) == TRUE, 1, 0)) %>%
  select(village, stime, mi,micRate, rdtRate, netCov, Netptwo, oneNet, slept_net_p) %>%
  group_by(village, stime) %>%
  summarize_all(funs(mean(., na.rm=TRUE))) %>%
  filter(is.na(village) == FALSE)


poly2 <- poly1
poly2 <- broom::tidy(poly2)
poly2$village <- as.numeric(poly2$id) - 54
poly2 <- left_join(poly2, md_time)

# function to extract legend only
g_legend <- function(a.gplot){ tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 

library(tidyr)
f1df <- gather(poly2, vars, rates, micRate, rdtRate)
f1df <- data.frame(f1df) %>%
    mutate(vars = ifelse(vars == 'rdtRate', 'RDT', 'Microscopy')) 
f1df$vars <- factor(f1df$vars, levels = c('RDT','Microscopy'))

#-- actual map function
library(scales)
library(ggmap)
library(gridExtra)
library(lattice)
library(RColorBrewer)
ggmap(map) +
    geom_polygon(aes(fill = rates, x = long, y = lat, group = group),
                 data = f1df[f1df$stime %in% c(1,2,3,4),] %>%
                     mutate(stime = ifelse(stime == 1, "Survey 1", ifelse(stime == 2, "Survey 2", ifelse(stime == 3, "Survey 3", "Survey 4")))),
                 alpha = 0.95,
                 color = "black",
                 size = 0.2) +
theme(axis.text.x = element_text(size=7, angle=45, hjust = 1)) +
facet_grid(vars ~ stime) +
scale_fill_gradientn(
                     colours=brewer.pal(10,"YlOrRd"),
                     #colours=rev(brewer.pal(10,"Spectral")),
                     breaks = quantile(0:1),
                     limits = c(0,1),
                     #midpoint = 0.5, 
                     name = paste0("rate")
                     )
ggsave("../../report/fig1.png")
# - - - - - - - - - - - - - - - - - - - - - #
## Figure 2. Smoothed RDT/MIC rate vs. Net Coverage
# - - - - - - - - - - - - - - - - - - - - - #
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
#-- make df that will allow facet_* by 
spline_k <- 4
f2df <- df %>%
    filter(county == 2) %>%
    gather(., vars, rates, micRate, rdtRate) %>%
    mutate(vars = ifelse(vars == "rdtRate", "RDT", "Microscopy"),
           Netptwo = ifelse(Netptwo == 1, ">50%", "<50%"))
f2a <- ggplot(f2df, aes(x = netCov, y = rates, color = as.factor(stime))) + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs="cs", k=spline_k)) + 
  facet_grid(vars ~ .)+
  #facet_grid(. ~ vars)+
  theme_light() + 
  ggtitle("A") +
  labs(color = "Survey Time") + 
  scale_color_manual(values=c("#000000", "#f20505","#f9d70b", '#f4a460')) + 
  theme(legend.position="bottom", axis.title.y = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_blank()
        ) +
  coord_cartesian(ylim = c(0, 0.6), xlim=c(0,1))

f2c <- ggplot(f2df, aes(x = as.factor(Netptwo), y = rates, fill = as.factor(stime))) + 
  geom_boxplot() +
  facet_grid(vars ~ .)+
  #facet_grid(. ~ vars)+
  labs(fill = "Survey Time") +
  scale_fill_manual(values=c("#000000", "#f20505","#f9d70b", '#f4a460')) + 
  ggtitle("C") +
  theme_light() + 
  theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank()) 

f2b <- ggplot(f2df, aes(x = slept_net_p, y = rates, color = as.factor(stime))) + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs="cs", k=spline_k)) + 
  facet_grid(vars ~ .)+
  #facet_grid(. ~ vars)+
  theme_light() + 
  labs(color = "Survey Time") + 
  ggtitle("B") +
  scale_color_manual(values=c("#000000", "#f20505","#f9d70b", '#f4a460')) + 
  theme(legend.position="bottom", axis.title.y = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 0.6), xlim=c(0,1))

mylegend<-g_legend(f2a)
f2 <- grid.arrange(
  arrangeGrob(f2a + theme(legend.position="none"),
              f2b + theme(legend.position="none"),
              f2c + theme(legend.position="none")
              ,nrow=1,
              left = textGrob("Rate", rot = 90, vjust = 1),
              bottom = textGrob("Coverate Rate")
              ),
  mylegend, nrow=2,heights=c(12, 1) 
  )
#library(cowplot)
#plot_grid(f2a, f2b, f2c, 
          #ncol=1,
          #axis='1',
          #labels = "AUTO",
          #align = 'v'
          #)
ggsave("./report/fig2.png")
