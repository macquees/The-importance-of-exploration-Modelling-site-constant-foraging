#Get ALL of the data into one matrix
library(car)
library(MPV)
fix(GANOVA)

setwd("~/SiteConstancy/Code/SimResults")

All_The_Data = read.csv("~/SiteConstancy/Code/All_The_Data")  # read in saved data instead of processing it again
All_The_Data = All_The_Data[,-1]  #remove the first column because it's just the index
attach(All_The_Data)

#the plots will go through all of the selection methods/distributions, and average over the simulations and/or bees
#landscapes = c("low_low","low_high", "high_low", "high_high")
landscapes = c("high_high")
generation_methods = c("random","exploration")
# generation_methods = c("random")
selection_methods = c("random","distance","type","energy")
# distributions = matrix(c("Steep","Shallow","Linear","nonLinear","Linear","nonLinear"),nrow=3,ncol=2,byrow=TRUE)



nsim = 20
n_bees = 100
n_fields_x = 20
n_fields_y = 20
n_fields = n_fields_x*n_fields_y


All_The_Data = data.frame(Landscape=character(),Generation=character(),Selection=character(),Distribution=character(),
                          BLB_Landscape=numeric(),BLB_mem=numeric(),BLB_loads=numeric(),
                          BLB_Fields_Visited=numeric(),WF_Fields_Visited=numeric(),Fields_Visited=numeric(),
                          MaxBLB_Fields_Visited=numeric(),MaxWF_Fields_Visited=numeric(),MaxFields_Visited=numeric(),
                          BLB_Flower_Visits=numeric(),WF_Flower_Visits=numeric(),Flower_Visits=numeric(),
                          TotalTime_Harvesting=numeric(),TotalTime_Scouting=numeric(),TotalTime_Returning=numeric(),
                          Time_Harvesting=numeric(),Time_Scouting=numeric(),Time_Returning=numeric(),
                          N_BLB_Fields_Visited=numeric(),N_Fields_Visited=numeric(),
                          stringsAsFactors=FALSE)
#leaving landscape and distribution in so as not to mess with the column numbers!!


Data_matrix_index = 1

for(i in 1:length(landscapes)){
  for(j in 1:length(generation_methods)){
    for(k in 1:length(selection_methods)){

        ##Read in the files for % in the landscape here
        #(resource only and whole landscape)
        filename = paste("LandscapePercentResourceOnly",generation_methods[j],
                         selection_methods[k],sep="_")
        Data_LandscapePercentResourceOnly = read.csv(filename,header=TRUE)
        filename = paste("LandscapePercent",generation_methods[j],
                         selection_methods[k],sep="_")
        Data_LandscapePercent = read.csv(filename,header=TRUE)

        ##Read in the file for total flower visits here
        filename = paste("TotalFlowersVisited",generation_methods[j],
                         selection_methods[k],sep="_")
        Data_TotalFlowersVisited = read.csv(filename,header=TRUE)

        for(n in 1:nsim){
          #Put the factors into the matrix here
          All_The_Data[Data_matrix_index,1:4] = c(landscapes[i],generation_methods[j],selection_methods[k],
                                                  "random")

          #Go through the rows of the % in the landscape files here (%blb = col 5)
          All_The_Data[Data_matrix_index,5] = 100*Data_LandscapePercentResourceOnly[n,2]
          All_The_Data[Data_matrix_index,25] = 100*Data_LandscapePercent[n,2]+100*Data_LandscapePercent[n,3]

          #read in the file for %blb mem spots here and process it (col 6)
          filename = paste("MemoryLocations",generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_MemoryLocations = read.csv(filename,header=TRUE)
          blb_mem_temp = length(which(Data_MemoryLocations[,4]==2))/length(Data_MemoryLocations[,4]) #calculate the percent of the memory points in blb
          All_The_Data[Data_matrix_index,6] = 100*blb_mem_temp     #store it in the matrix

          #read in the file for %blb load here and process it (col 7)
          filename = paste("BLBPollenLoadPercent",generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_BLBPollenLoadPercent = read.csv(filename,header=TRUE)
          Data_BLBPollenLoadPercent[is.na(Data_BLBPollenLoadPercent)]=0             #turn NAs into 0 (these bees didn't collect anything at all, I guess)
          mean_temp = apply(Data_BLBPollenLoadPercent[,2:7],2,mean) #calculate the average
          blb_load_temp = mean(mean_temp)
          All_The_Data[Data_matrix_index,7] = blb_load_temp     #store it in the matrix

          #read in the files for blb and all field visits here and process it
          #prop blb = col 8, prop wf = col 9, prop all = col 10
          filename = paste("BLBFlowerVisitLocations",generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_BLBFlowerVisitLocations = dget(filename)
          filename = paste("TotalFlowerVisitLocation",generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_TotalFlowerVisitLocations = dget(filename)
          #count the number of visited fields (blb and total)
          visited_blb_fields = length(which(Data_BLBFlowerVisitLocations>0))
          All_The_Data[Data_matrix_index,23] = visited_blb_fields
          visited_fields = length(which(Data_TotalFlowerVisitLocations>0))
          All_The_Data[Data_matrix_index,24] = visited_fields
          visited_wf_fields = visited_fields - visited_blb_fields
          #count the total number of blb fields
          total_blb_fields = Data_LandscapePercent[n,2]*n_fields
          total_wf_fields = Data_LandscapePercent[n,3]*n_fields
          total_fields = total_blb_fields + total_wf_fields
          #calculate the proportions and save the data in the right format
          All_The_Data[Data_matrix_index,8] = 100*visited_blb_fields/total_blb_fields  #the percent of blb fields visited
          All_The_Data[Data_matrix_index,9] = 100*visited_wf_fields/total_wf_fields
          All_The_Data[Data_matrix_index,10] = 100*visited_fields/total_fields

          #go through the rows of the total flower visits file here
          #blb = 14, wf = 15, all = 16
          All_The_Data[Data_matrix_index,14] = Data_TotalFlowersVisited[n,3]
          All_The_Data[Data_matrix_index,15] = Data_TotalFlowersVisited[n,4]
          All_The_Data[Data_matrix_index,16] = Data_TotalFlowersVisited[n,2]

          #read in the file for HSR times here, and process it
          #H=17, S=18, R=19
          #%H=20, %S=21, %R=22
          filename = paste("HSRTimes",generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_HSRTimes = read.csv(filename,header=TRUE)
          Data_HSRTimes = Data_HSRTimes[2:4]
          All_The_Data[Data_matrix_index,17:19] = apply(Data_HSRTimes,2,mean)   #get the average of all bees for this simulation (columns are H,S,R)
          All_The_Data[Data_matrix_index,20:22] = 100*apply(Data_HSRTimes/rowSums(Data_HSRTimes),2,mean)   #get the average of all bees for this simulation (columns are H,S,R)

          Data_matrix_index = Data_matrix_index+1    #go to the next row in the matrix
        }
    }
  }
}

All_The_Data$ResourceLevel = substr(x=All_The_Data$Landscape,start=1,stop=4)
All_The_Data$ResourceLevel = gsub("_","",All_The_Data$ResourceLevel)
All_The_Data$BLBLevel  = substr(x=All_The_Data$Landscape,start=5,stop=10)
All_The_Data$BLBLevel = gsub("_","",All_The_Data$BLBLevel)
All_The_Data$ResourceBlbGenerationSelection = paste(All_The_Data$ResourceLevel,All_The_Data$BLBLevel,All_The_Data$Generation,All_The_Data$Selection,sep="_")

All_The_Data$Landscape = factor(All_The_Data$Landscape,levels=unique(All_The_Data$Landscape))
All_The_Data$Generation = factor(All_The_Data$Generation,levels=unique(All_The_Data$Generation))
All_The_Data$Selection = factor(All_The_Data$Selection,levels=unique(All_The_Data$Selection))
All_The_Data$Distribution = factor(All_The_Data$Distribution,levels=unique(All_The_Data$Distribution))
All_The_Data$ResourceLevel = factor(All_The_Data$ResourceLevel,levels=unique(All_The_Data$ResourceLevel))
All_The_Data$BLBLevel = factor(All_The_Data$BLBLevel,levels=unique(All_The_Data$BLBLevel))
All_The_Data$ResourceBlbGenerationSelection = factor(All_The_Data$ResourceBlbGenerationSelection,levels=unique(All_The_Data$ResourceBlbGenerationSelection))

All_The_Data$BLB_mem_diff = All_The_Data$BLB_mem-All_The_Data$BLB_Landscape
All_The_Data$BLB_loads_diff = All_The_Data$BLB_loads-All_The_Data$BLB_Landscape

All_The_Data$Time_Total = All_The_Data$TotalTime_Scouting+All_The_Data$TotalTime_Harvesting+All_The_Data$TotalTime_Returning


attach(All_The_Data)

write.csv(All_The_Data,"~/SiteConstancy/Code/All_The_Data")

###########################################
# #this file has the color/shape/level information
# TreatmentLevels = read.csv("~/SiteConstancy/Code/TreatmentLevels_new2")
# attach(TreatmentLevels)

#colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# black, goldenrod, light blue, green, yellow, dark blue, orange, mauve

Color = c(cbbPalette[2],cbbPalette[3],cbbPalette[4],cbbPalette[8])  
#colors for selection method (distance, energy, random, type)
# or (random, distance, type, energy)
# Shape = c(16,16,16,16,17,17,17,17)  #exploration is dot, random generation is triangle
Shape = c(17,17,17,17,16,16,16,16)  #random generation is triangle, exploration is dot 
# alphabetical order or not when ordered and split??

#first make the data for number of blb flowers visited
Resource_FlowersVisited= data.frame(group = All_The_Data$ResourceBlbGenerationSelection,FlowersVisited = All_The_Data$BLB_Flower_Visits)
dataset = Resource_FlowersVisited
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_FlowersVisited,type="hist",center=FALSE,x.axis.label="Number of Blueberry Flowers Visited",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape,
       col=as.character(Color),cex=2.5)  #add the points on the x-axis
# points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

# #first make the data for percent of foraging sites in blb (difference from landscape)
# Resource_MemPoints_Diff = data.frame(group = All_The_Data$ResourceBlbGenerationSelection,MemPoints_Diff = All_The_Data$BLB_mem_diff)
# dataset = Resource_MemPoints_Diff
# dataset <- dataset[order(dataset[, 1]), ]
# data.split <- split(dataset[, 2], dataset[, 1])
# treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
# #then do the plot
# GANOVA(Resource_MemPoints_Diff,type="hist",center=FALSE,x.axis.label="Percent of Foraging Sites in Blueberry \n (Difference from Landscape)",
#        char=".")
# points(treatments,rep(0,length(treatments)),pch=Shape,col=as.character(Color),cex=1.5)  #add the points on the x-axis
# # points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# # abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for percent of foraging sites in blb
Resource_MemPoints = data.frame(group = All_The_Data$ResourceBlbGenerationSelection,MemPoints = All_The_Data$BLB_mem)
dataset = Resource_MemPoints
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_MemPoints,type="hist",center=FALSE,x.axis.label="Percent of Foraging Sites in Blueberry",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape,
       col=as.character(Color),cex=2.5)  #add the points on the x-axis
# points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments


#first make the data for percent of blb in resource loads
Resource_Loads = data.frame(group = All_The_Data$ResourceBlbGenerationSelection,Loads = All_The_Data$BLB_loads)
dataset = Resource_Loads
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Loads,type="hist",center=FALSE,x.axis.label="Percent of Blueberry in Resource Loads",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape,
       col=as.character(Color),cex=2.5)  #add the points on the x-axis
# points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for harvesting time
Resource_Harvesting = data.frame(group = All_The_Data$ResourceBlbGenerationSelection,Harvesting = All_The_Data$Time_Harvesting)
dataset = Resource_Harvesting
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Harvesting,type="hist",center=FALSE,x.axis.label="Percent of Time Spent Harvesting",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape,
       col=as.character(Color),cex=2.5)  #add the points on the x-axis
# points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments



#first make the data for Total time
Resource_Total = data.frame(group = All_The_Data$ResourceBlbGenerationSelection,Returning = All_The_Data$Time_Total/60) #divide by 60 to put it in minutes
dataset = Resource_Total
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Total,type="hist",center=FALSE,x.axis.label="Total Time (minutes)",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape,
       col=as.character(Color),cex=2.5)  #add the points on the x-axis
# points(treatments,Level,pch=Shape,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments


plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

legend("topleft",legend=c("exploration","random"),
       col=1, bty="n",
       pch=c(16,17),title="Site Generation Method")
# legend("left",legend=c("distance to point","energy","random","type of flowers"),
#        col=as.character(Color), bty="n",
#        pch=15,title="Point Selection Method")
legend("bottomleft",legend=c("random","distance to point","type of flowers","energy"),
       col=as.character(Color), bty="n",
       pch=15,title="Site Selection Method")

# ######################### ANOVA ###################################

####################### Single Variable Model Fitting #################################

## number of blb flowers visited
model_flowers = lm(BLB_Flower_Visits~Generation+Selection+Generation:Selection)
anova_flowers = Anova(model_flowers,type="II")
plot(model_flowers$residuals~model_flowers$fitted.values)

model_flowers_reg = lm(BLB_Flower_Visits~Selection)
summary(model_flowers_reg)

## percent of foraging sites in blb
model_sites = lm(BLB_mem~Generation+Selection+Generation:Selection)
anova_sites = Anova(model_sites,type="II")
plot(model_sites$residuals~model_sites$fitted.values)

model_sites_reg = lm(BLB_mem~Selection)
summary(model_sites_reg)


model_loads = lm(BLB_loads~Generation+Selection+Generation:Selection)
anova_loads = Anova(model_loads,type="II")
plot(model_loads$residuals~model_loads$fitted.values)

model_loads_reg = lm(BLB_loads~Selection)
summary(model_loads_reg) 

model_harvesting = lm(Time_Harvesting~Generation+Selection+Generation:Selection)
anova_harvesting = Anova(model_harvesting,type="II")
plot(model_harvesting$residuals~model_harvesting$fitted.values)

model_harvesting_reg = lm(Time_Harvesting~Generation+Selection+Generation:Selection)
summary(model_harvesting_reg)


model_totaltime = lm(Time_Total~Generation+Selection+Generation:Selection)
anova_total_time = Anova(model_totaltime,type="II")
plot(model_totaltime$residuals~model_totaltime$fitted.values)


#store the anova results in a data frame
ANOVA_Results = data.frame(terms = c("Generation","Selection","Generation:Selection","Residual"),
                           Flowers_Visited=numeric(4), 
                           Sites = numeric(4), 
                           Loads = numeric(4),
                           Time_Harvesting = numeric(4), 
                           Total_Time=numeric(4))

ANOVA_Results[,2] = 100*anova_flowers$`Sum Sq`/sum(anova_flowers$`Sum Sq`)
ANOVA_Results[,3] = 100*anova_sites$`Sum Sq`/sum(anova_sites$`Sum Sq`)
ANOVA_Results[,4] = 100*anova_loads$`Sum Sq`/sum(anova_loads$`Sum Sq`)
ANOVA_Results[,5] = 100*anova_harvesting$`Sum Sq`/sum(anova_harvesting$`Sum Sq`)
ANOVA_Results[,6] = 100*anova_total_time$`Sum Sq`/sum(anova_total_time$`Sum Sq`)
colnames(ANOVA_Results) = c("terms", "Flowers Visited","Foraging Sites", "Resource Loads", "Harvesting", "Total Time")
round(ANOVA_Results[,2:6],2)
