#useful commonly used functions/commands

########### dates and times #################
HW_div$Date=as.Date(HW_div$Date, format="%m/%d/%Y")
hw1.1$time <- format(as.POSIXct(strptime(hw1.1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="MST")) ,format = "%H:%M")# extracting time from date time
hw1.1$date.time <- as.POSIXct(hw1.1$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
hw1.1$julian <- julian(month,day2,year, origin=c(1,1,2016))# converting date into decimal days, with origin of 1/1/16
HW_div$hr <- as.numeric(substr(HW_div$Date.Time,start=nchar(as.character(HW_div$Date.Time))-4, stop=nchar(as.character(HW_div$Date.Time))-3))#adding a column that is the hour (time) that measurement was taken
strptime()#assigning character string to date
as.POSIXlt()#stores time in hours minutes etc
as.POSIXct#stores time in seconds since 1970
month()#extracts month from date, also can to minutes, year etc. 


hw1.1 <- hw1.1 %>% 
    mutate(
        date.time = ymd_hms(TIMESTAMP, tz = "MST"),
        date = as.Date(date.time),
        hr = hour(date.time),
        julian = doy.h_date_hr(date = date, hr = hr + minute(date.time)/60)
    )
########### end: dates and times ###############

####### data frames ##########
hw1.1[,c(2:20)] <- lapply(hw1.1[,c(2:20)], as.numeric)#converting many columns to numeric
Tnorm=data.frame(matrix(NA,ncol=6,nrow=nrow(hw1.2)))#making empty dataframe
h <- do.call(rbind,g)#binding list of dataframes
ibut_join1 <- ibut_hang1 %>% 
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by=c('date.time',"unit")), .)#joining list of dataframes
###### end data frames ######



dm=apply(simplify2array(dall), c(1,2), mean, na.rm=T)#creating matrix that is an element wise mean of the plot level matrices.
div=subset(div, div$Date >= "2017-01-01") #date range
unname(DTdry[i])#removing the "name" from a named object e.g. from using sapply

#########reading in files#############################
source("~/Analysis/Rfunctions/assign_trmts_functions.R")
source("~/Analysis/Rfunctions/general_functions.v1.R")# source code including function used later
list.files(path = ".")#files in directory
HW_div=read.csv("Hardware_Diviner_processed_5.csv", skip = 1, header = T)
read.delim("C:\\Users\\grad\\Documents\\Data\\Hardware\\CR1000\\Hardware1_plot4\\HW1_plot4_matric.dat", skip=1,header=T, sep=",")

dput()#saving r object with attributes
dget()# loading R object

########### end: reading in files#####################

####### reviewing data after reading it in ##########
object.size()#object size
summary()
table()#for categorical/factor variable

#######
#~~~~~
library(reshape2) # loading package so can use melt function
# reshaping the data to make a "depth" column and a "moisture" column only...long form
HW_div.m1=melt(HW_div, measure.vars = c("X10cm", "X20cm","X30cm", "X40cm","X50cm","X60cm","X70cm","X80cm","X90cm","X100cm"), variable_name = "depth_name")
#~~~~~~~~~~
detach("package:reshape2", unload=TRUE)#"turning off" the package
#~~~~~~~~~~

#~~~~~~~~~~~
c11$mean=rowMeans(c11[4:6], na.rm=T)#row means
#~~~~~
d.max$xcm <- ifelse(d.max[,3]<55, d.max[,3]*10/55, 10)
#
d.max <- aggregate(moisture~ plot + depth, data=HW_div.c1, FUN=max, na.rm = TRUE, na.action = NULL)
#~~~~~
snow.clark$md <- as.integer(substr(as.character(snow.clark$date),start=5, stop=8))#extracting the month/day
#
grepl(needle,haystack,fixed=T)#finding if text is found in a character. 
#~~~~~~~~
cumsum(snow1[snow1$year==years[i],"trt1"])# cumulative sums of a vector
#~~~~
na.zero() #filling in 0's
test <- function(x,name){
    switch(name,
           min = min(x),
           max = max(x)
    )
}
test(1:4,"min"
#~~~~~~~~~
library(matrixStats)
colMedians(as.matrix(year.snow1))#column wise medians
#~~~~~~
year.snow1 <- merge(year.snow1,year.trt10) #merging dataframes 
#~~~~~
assign(paste0("c",plot),c)#assign variable name to a value
#~~~~~~~~~~
ecdf(year.snow$SNOW)(c(13,12,10,8,6,3))# determining the percentiles of the number of snow events per year of the control through +10 trmts. ecdf makes a distribution from the data, and this is used as a function
#~~~~~~~~~~


#graphics########################################################
#legend 
legend("top", legend = c("mean of all covariates", "minimum depth", "maximimum depth", "minimum day of year", "maximum day of year"), col =c("black", "blue", "blue", "red", "red"), lty = c(1,3,1,3,1), ncol=2, xpd = NA, inset= c(-0.2), lwd=4, cex =0.8, bty="n")
#
#~
pdf(paste("trmt", trmt[1],"plot",  plot[i], ".pdf", sep=""), width=9, height = 6) #printing plot to pdf
plot(...)
dev.off()
#~~
### ggplot
ggplot(data=hw1_met_dly1)+
    geom_line(aes(x=date,y=AirTC_in_mean, 
                  color=paste("shelter T, mean =", round(Tmeans["AirTC_in_mean"],2))))+
    geom_line(aes(x=date,y=AirTC_out_mean, 
                  color=paste("ambient T, mean =", round(Tmeans["AirTC_out_mean"],2))))+
    scale_color_manual(values=c("blue","black"))+
    theme_classic(base_size = 16)+
    xlab("Date")+
    ylab("air temperature (C)")+
    labs(title="Daily mean temperature in and outside plot 4 shelter")+
    theme(legend.title=element_blank(),legend.position="top",legend.direction="vertical")

cut_width() #creating binned data

ggplot(twr_3,aes(x=doy,y=NDVI))+
    geom_point(aes(color=year))+
    geom_smooth(aes(color=year),method ="lm",formula=y~x+I(x^2),se=F)+
    facet_wrap(~trmt)+
    theme_bw(base_size = 16)+
    ylab("NDVI (mean across subplots)")+
    xlab("day of year")+
    scale_color_manual(values=c("blue","black"))+
    labs(title="Hardware, NDVI vs date by treatment (2016 vs 2017)")
ggsave("scatterplots_NDVI/HW_NDVI_vs_date_by_trmt_2016_vs_17_v1.pdf")


dat_text <- data.frame(
    label = c("4 cylinders", "6 cylinders", "8 cylinders","test"),
    spp   = unique(por5_yrsp_avgnocc$spp),
    x     = rep(7,4),
    y     = rep(350,4)
)

ggplot(por5_yrsp_avgnocc,aes(x= trmt, y=conductance))+
    geom_point(aes(color = year))+
    geom_smooth(aes(color = year), method ="lm",se=F)+
    facet_wrap(~spp)+
    theme_bw(base_size = 16)+
    ylab("conductance [mmol/(m²·s)]")+
    xlab("treatment")+
    labs(title="Hardware, porometer data by species (GR = grass)\n(values averaged by plot but not across years)")+
    geom_text(
        data    = dat_text,
        mapping = aes(x = x, y = y, label = label)
    )

colfun <- colorRampPalette(c("blue","orange","red"))
clrs <- colfun(7)
clrs <- c("black",clrs)
ggplot(data = clean4_sensorlevel_longform)+
    geom_point(aes(x = doy.h, y = mm_growth_trmt, color = trmt), alpha = 0.1)+
    scale_color_manual(values = clrs)+
    coord_cartesian(ylim = c(-.5,2))+
    xlab("Day Number (starting 1/1/2016)")+
    ylab("stem growth (mm)")+
    labs(title = "HW Ranch, dendrometer data, by treatment \n(shrub level averages by treatment)")+
    guides(colour = guide_legend(override.aes = list(alpha = 1)))+
    theme_bw(base_size = 16)

pdf("figures/histograms/HW_porometer_hist_combined_v1.pdf", width = 13, height = 8.5)
l_hist
dev.off()

############### from online R course ######
dput()#saving r object with attributes
dget()# loading R object 
#### from hands on programming with R
expand.grid()#every combo of n vectors
options(stringsAsFactors = F)#global setting, useful if reading in multiple data frame