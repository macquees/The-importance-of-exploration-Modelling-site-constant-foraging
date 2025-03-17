#Get ALL of the data into one matrix
library(MPV)

setwd("C:/Users/UCD/Documents/SiteConstancy/Code/2020-02-04")

#the plots will go through all of the selection methods/distributions, and average over the simulations and/or bees
#landscapes = c("low_low","low_high", "high_low", "high_high")
landscapes = c("high_high")
generation_methods = c("random","exploration")
selection_methods = c("random","distance","type","energy")
distributions = matrix(c("Steep","Shallow","Linear","nonLinear","Linear","nonLinear"),nrow=3,ncol=2,byrow=TRUE)



nsim = 20
n_bees = 100
n_fields_x = 20
n_fields_y = 20
n_fields = n_fields_x*n_fields_y


# All_The_Data = matrix(nrow=56*20,ncol=22)

# colnames(All_The_Data) = c("Landscape","Generation","Selection","Distribution",
#                            "%BLB_Landscape","%BLB_mem","%BLB_loads",
#                            "%BLB_Fields_Visited","%WF_Fields_Visited","%Fields_Visited",
#                            "MaxBLB_Fields_Visited","MaxWF_Fields_Visited","MaxFields_Visited",
#                            "BLB_Flower_Visits","WF_Flower_Visits","Flower_Visits",
#                            "TotalTime_Harvesting","TotalTime_Scouting","TotalTime_Returning",
#                            "%Time_Harvesting","%Time_Scouting","%Time_Returning")

All_The_Data = data.frame(Landscape=character(),Generation=character(),Selection=character(),Distribution=character(),
                          BLB_Landscape=numeric(),BLB_mem=numeric(),BLB_loads=numeric(),
                          BLB_Fields_Visited=numeric(),WF_Fields_Visited=numeric(),Fields_Visited=numeric(),
                          MaxBLB_Fields_Visited=numeric(),MaxWF_Fields_Visited=numeric(),MaxFields_Visited=numeric(),
                          BLB_Flower_Visits=numeric(),WF_Flower_Visits=numeric(),Flower_Visits=numeric(),
                          TotalTime_Harvesting=numeric(),TotalTime_Scouting=numeric(),TotalTime_Returning=numeric(),
                          Time_Harvesting=numeric(),Time_Scouting=numeric(),Time_Returning=numeric(),
                          N_BLB_Fields_Visited=numeric(),N_Fields_Visited=numeric(),
                          Resource_Landscape,stringsAsFactors=FALSE)


Data_matrix_index = 1

for(i in 1:length(landscapes)){
  for(j in 1:length(generation_methods)){
    for(k in 1:length(selection_methods)){

      if(k == 1){ #the random selection 
        ##Read in the files for % in the landscape here 
        #(resource only and whole landscape)
        filename = paste("LandscapePercentResourceOnly_Landscape",landscapes[i],generation_methods[j],
                         selection_methods[k],sep="_")
        Data_LandscapePercentResourceOnly = read.csv(filename,header=TRUE)
        filename = paste("LandscapePercent_Landscape",landscapes[i],generation_methods[j],
                         selection_methods[k],sep="_")
        Data_LandscapePercent = read.csv(filename,header=TRUE)
        
        ##Read in the file for total flower visits here
        filename = paste("TotalFlowersVisited_Landscape",landscapes[i],generation_methods[j],
                         selection_methods[k],sep="_")
        Data_TotalFlowersVisited = read.csv(filename,header=TRUE)
        
        for(n in 1:nsim){
          #Put the factors into the matrix here
          All_The_Data[Data_matrix_index,1:4] = c(landscapes[i],generation_methods[j],selection_methods[k],
                                                  "random")
          
          #Go through the rows of the % in the landscape files here (%blb = col 5)
          All_The_Data[Data_matrix_index,5] = 100*Data_LandscapePercentResourceOnly[n,2]
          All_The_Data[Data_matrix_index,27] = 100*Data_LandscapePercent[n,2]+100*Data_LandscapePercent[n,3]
          
          #read in the file for %blb mem spots here and process it (col 6)
          filename = paste("MemoryLocations_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_MemoryLocations = read.csv(filename,header=TRUE)
          blb_mem_temp = length(which(Data_MemoryLocations[,4]==2))/length(Data_MemoryLocations[,4]) #calculate the percent of the memory points in blb
          All_The_Data[Data_matrix_index,6] = 100*blb_mem_temp     #store it in the matrix
          
          #read in the file for %blb load here and process it (col 7)
          filename = paste("BLBPollenLoadPercent_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_BLBPollenLoadPercent = read.csv(filename,header=TRUE)
          Data_BLBPollenLoadPercent[is.na(Data_BLBPollenLoadPercent)]=0             #turn NAs into 0 (these bees didn't collect anything at all, I guess)
          mean_temp = apply(Data_BLBPollenLoadPercent[,2:7],2,mean) #calculate the average
          blb_load_temp = mean(mean_temp)
          All_The_Data[Data_matrix_index,7] = blb_load_temp     #store it in the matrix
          
          #read in the files for blb and all field visits here and process it
          #prop blb = col 8, prop wf = col 9, prop all = col 10
          filename = paste("BLBFlowerVisitLocations_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_BLBFlowerVisitLocations = dget(filename)
          filename = paste("TotalFlowerVisitLocation_Landscape",landscapes[i],generation_methods[j],
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
          filename = paste("HSRTimes_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],"Simulation",n,sep="_")
          Data_HSRTimes = read.csv(filename,header=TRUE)
          Data_HSRTimes = Data_HSRTimes[2:4]
          All_The_Data[Data_matrix_index,17:19] = apply(Data_HSRTimes,2,mean)   #get the average of all bees for this simulation (columns are H,S,R)
          All_The_Data[Data_matrix_index,20:22] = 100*apply(Data_HSRTimes/rowSums(Data_HSRTimes),2,mean)   #get the average of all bees for this simulation (columns are H,S,R)
          
          Data_matrix_index = Data_matrix_index+1    #go to the next row in the matrix
          
        }
        
      }else{#the distance/type/energy selection
        for(m in 1:2){  #probability distributions
          #go through the correct row of the distributions matrix
          ##Read in the files for % in the landscape here 
          #(resource only and whole landscape)
          filename = paste("LandscapePercentResourceOnly_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],distributions[k-1,m],sep="_")
          Data_LandscapePercentResourceOnly = read.csv(filename,header=TRUE)
          filename = paste("LandscapePercent_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],distributions[k-1,m],sep="_")
          Data_LandscapePercent = read.csv(filename,header=TRUE)
          
          ##Read in the file for total flower visits here
          filename = paste("TotalFlowersVisited_Landscape",landscapes[i],generation_methods[j],
                           selection_methods[k],distributions[k-1,m],sep="_")
          Data_TotalFlowersVisited = read.csv(filename,header=TRUE)
          
          for(n in 1:nsim){
            #Put the factors into the matrix here
            All_The_Data[Data_matrix_index,1:4] = c(landscapes[i],generation_methods[j],selection_methods[k],
                                                    distributions[k-1,m])
            
            #Go through the rows of the % in the landscape files here (%blb = col 5)
            All_The_Data[Data_matrix_index,5] = 100*Data_LandscapePercentResourceOnly[n,2]
            
            #read in the file for %blb mem spots here and process it (col 6)
            filename = paste("MemoryLocations_Landscape",landscapes[i],generation_methods[j],
                             selection_methods[k],distributions[k-1,m],"Simulation",n,sep="_")
            Data_MemoryLocations = read.csv(filename,header=TRUE)
            blb_mem_temp = length(which(Data_MemoryLocations[,4]==2))/length(Data_MemoryLocations[,4]) #calculate the percent of the memory points in blb
            All_The_Data[Data_matrix_index,6] = 100*blb_mem_temp     #store it in the matrix
            
            #read in the file for %blb load here and process it (col 7)
            filename = paste("BLBPollenLoadPercent_Landscape",landscapes[i],generation_methods[j],
                             selection_methods[k],distributions[k-1,m],"Simulation",n,sep="_")
            Data_BLBPollenLoadPercent = read.csv(filename,header=TRUE)
            Data_BLBPollenLoadPercent[is.na(Data_BLBPollenLoadPercent)]=0             #turn NAs into 0 (these bees didn't collect anything at all, I guess)
            mean_temp = apply(Data_BLBPollenLoadPercent[,2:7],2,mean) #calculate the average
            blb_load_temp = mean(mean_temp)
            All_The_Data[Data_matrix_index,7] = blb_load_temp     #store it in the matrix
            
            #read in the files for blb and all field visits here and process it
            #prop blb = col 8, prop wf = col 9, prop all = col 10
            filename = paste("BLBFlowerVisitLocations_Landscape",landscapes[i],generation_methods[j],
                             selection_methods[k],distributions[k-1,m],"Simulation",n,sep="_")
            Data_BLBFlowerVisitLocations = dget(filename)
            filename = paste("TotalFlowerVisitLocation_Landscape",landscapes[i],generation_methods[j],
                             selection_methods[k],distributions[k-1,m],"Simulation",n,sep="_")
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
            All_The_Data[Data_matrix_index,8] = 100*visited_blb_fields/total_blb_fields  #the percent of blb in that simulation
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
            filename = paste("HSRTimes_Landscape",landscapes[i],generation_methods[j],
                             selection_methods[k],distributions[k-1,m],"Simulation",n,sep="_")
            Data_HSRTimes = read.csv(filename,header=TRUE)
            Data_HSRTimes = Data_HSRTimes[2:4]
            All_The_Data[Data_matrix_index,17:19] = apply(Data_HSRTimes,2,mean)   #get the average of all bees for this simulation (columns are H,S,R)
            All_The_Data[Data_matrix_index,20:22] = 100*apply(Data_HSRTimes/rowSums(Data_HSRTimes),2,mean)   #get the average of all bees for this simulation (columns are H,S,R)
            
            Data_matrix_index = Data_matrix_index+1    #go to the next row in the matrix
            
          }
          
        }
      }
    }
  }
}

All_The_Data$ResourceLevel = substr(x=All_The_Data$Landscape,start=1,stop=4)
All_The_Data$ResourceLevel = gsub("_","",All_The_Data$ResourceLevel)
All_The_Data$BLBLevel  = substr(x=All_The_Data$Landscape,start=5,stop=10)
All_The_Data$BLBLevel = gsub("_","",All_The_Data$BLBLevel)
All_The_Data$SelectionDistribution = paste(All_The_Data$Selection,All_The_Data$Distribution,sep="_")
All_The_Data$ResourceBlbGenerationSelectionDistribution = paste(All_The_Data$ResourceLevel,All_The_Data$BLBLevel,All_The_Data$Generation,All_The_Data$Selection,All_The_Data$Distribution,sep="_")

All_The_Data$Landscape = factor(All_The_Data$Landscape,levels=unique(All_The_Data$Landscape))
All_The_Data$Generation = factor(All_The_Data$Generation,levels=unique(All_The_Data$Generation))
All_The_Data$Selection = factor(All_The_Data$Selection,levels=unique(All_The_Data$Selection))
All_The_Data$Distribution = factor(All_The_Data$Distribution,levels=unique(All_The_Data$Distribution))
All_The_Data$ResourceLevel = factor(All_The_Data$ResourceLevel,levels=unique(All_The_Data$ResourceLevel))
All_The_Data$BLBLevel = factor(All_The_Data$BLBLevel,levels=unique(All_The_Data$BLBLevel))
All_The_Data$SelectionDistribution = factor(All_The_Data$SelectionDistribution,levels=unique(All_The_Data$SelectionDistribution))
All_The_Data$ResourceBlbGenerationSelectionDistribution = factor(All_The_Data$ResourceBlbGenerationSelectionDistribution,levels=unique(All_The_Data$ResourceBlbGenerationSelectionDistribution))

All_The_Data$BLB_mem_diff = All_The_Data$BLB_mem-All_The_Data$BLB_Landscape
All_The_Data$BLB_loads_diff = All_The_Data$BLB_loads-All_The_Data$BLB_Landscape

All_The_Data$Time_Total = All_The_Data$TotalTime_Scouting+All_The_Data$TotalTime_Harvesting+All_The_Data$TotalTime_Returning


attach(All_The_Data)

write.csv(All_The_Data,"/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/All_The_Data")

###########################################
#this file has the color/shape/level information
TreatmentLevels = read.csv("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/TreatmentLevels")
attach(TreatmentLevels)

# par(mfrow=c(2,4))

#first make the data for percent of blb fields visited
Resource_FieldsVisited= data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,FieldsVisited = All_The_Data$BLB_Fields_Visited)
dataset = Resource_FieldsVisited
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_FieldsVisited,type="hist",center=FALSE,x.axis.label="Percent of Blueberry Fields Visited",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for number of blb flowers visited
Resource_FlowersVisited= data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,FlowersVisited = All_The_Data$BLB_Flower_Visits)
dataset = Resource_FlowersVisited
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_FlowersVisited,type="hist",center=FALSE,x.axis.label="Number of Blueberry Flowers Visited",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for percent of foraging sites in blb (difference from landscape)
Resource_MemPoints_Diff = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,MemPoints_Diff = All_The_Data$BLB_mem_diff)
dataset = Resource_MemPoints_Diff
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_MemPoints_Diff,type="hist",center=FALSE,x.axis.label="Percent of Foraging Sites in Blueberry \n (Difference from Landscape)",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for percent of foraging sites in blb
Resource_MemPoints = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,MemPoints = All_The_Data$BLB_mem)
dataset = Resource_MemPoints
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_MemPoints,type="hist",center=FALSE,x.axis.label="Percent of Foraging Sites in Blueberry",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

# #first make the data for percent of blb in resource loads
# Resource_Loads_Diff= data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Loads_Diff = All_The_Data$BLB_loads_diff)
# dataset = Resource_Loads_Diff
# dataset <- dataset[order(dataset[, 1]), ]
# data.split <- split(dataset[, 2], dataset[, 1])
# treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
# #then do the plot
# GANOVA(Resource_Loads_Diff,type="hist",center=FALSE,x.axis.label="Percent of Blueberry in Resource Loads  \n (Difference from Landscape)",
#        char=".")
# points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
# points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
# abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for percent of blb in resource loads
Resource_Loads = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Loads = All_The_Data$BLB_loads)
dataset = Resource_Loads
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Loads,type="hist",center=FALSE,x.axis.label="Percent of Blueberry in Resource Loads",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for harvesting time
Resource_Harvesting = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Harvesting = All_The_Data$Time_Harvesting)
dataset = Resource_Harvesting
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Harvesting,type="hist",center=FALSE,x.axis.label="Average Percent of Time Spent Harvesting",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

#first make the data for scouting time
Resource_Scouting = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Scouting = All_The_Data$Time_Scouting)
dataset = Resource_Scouting
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Scouting,type="hist",center=FALSE,x.axis.label="Average Percent of Time Spent Scouting",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments


#first make the data for Returning time
Resource_Returning = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Returning = All_The_Data$Time_Returning)
dataset = Resource_Returning
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Returning,type="hist",center=FALSE,x.axis.label="Average Percent of Time Spent Returning",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments


#first make the data for Total time
Resource_Total = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Returning = All_The_Data$Time_Total)
dataset = Resource_Total
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(Resource_Total,type="hist",center=FALSE,x.axis.label="Average Total Time (for all trips)",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments

# ######################### Scatter Plots ###################################
# ######################### Normality and equal Variance Tests ##############
# 
# ResourceLevel1 = which(All_The_Data$ResourceLevel=="low")
# ResourceLevel2 = which(All_The_Data$ResourceLevel=="high")
# BLBLevel1 = which(All_The_Data$BLBLevel=="low")
# BLBLevel2 = which(All_The_Data$BLBLevel=="high")
# Generation1 = which(All_The_Data$Generation=="random")
# Generation2 = which(All_The_Data$Generation=="exploration")
# Selection1 = which(All_The_Data$SelectionDistribution=="random_random")
# Selection2 = which(All_The_Data$SelectionDistribution=="distance_Steep")
# Selection3 = which(All_The_Data$SelectionDistribution=="distance_Shallow")
# Selection4 = which(All_The_Data$SelectionDistribution=="type_Linear")
# Selection5 = which(All_The_Data$SelectionDistribution=="type_NonLinear")
# Selection6 = which(All_The_Data$SelectionDistribution=="energy_Linear")
# Selection7 = which(All_The_Data$SelectionDistribution=="energy_nonLinear")
# 
# GANOVA_subData = data.frame(ResourceLevel=All_The_Data$ResourceLevel,BLBLevel=All_The_Data$BLBLevel,
#                             Generation=All_The_Data$Generation,SelectionDistribution=All_The_Data$SelectionDistribution,
#                             BLB_Flower_Visits=All_The_Data$BLB_Flower_Visits,BLB_Fields_Visited=All_The_Data$BLB_Fields_Visited,
#                             BLB_mem=All_The_Data$BLB_mem_diff,BLB_loads=All_The_Data$BLB_loads_diff,
#                             Time_Harvesting=All_The_Data$Time_Harvesting,Time_Scouting=All_The_Data$Time_Scouting,
#                             Time_Returning=All_The_Data$Time_Returning,Total_Time = All_The_Data$Time_Total,stringsAsFactors=FALSE)
# 
# 
# GANOVA_subData$FactorGroupA = as.character(paste(GANOVA_subData$ResourceLevel,"r",sep=""))
# GANOVA_subData$FactorGroupB = as.character(paste(GANOVA_subData$BLBLevel,"b",sep=""))
# GANOVA_subData$FactorGroupC = as.character(GANOVA_subData$Generation)
# GANOVA_subData$FactorGroupD = as.character(GANOVA_subData$SelectionDistribution)
# GANOVA_subData$FactorGroupE = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$FactorGroupB,sep="")
# GANOVA_subData$FactorGroupF = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$Generation,sep="")
# GANOVA_subData$FactorGroupG = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupH = paste(GANOVA_subData$FactorGroupB,GANOVA_subData$Generation,sep="")
# GANOVA_subData$FactorGroupI = paste(GANOVA_subData$FactorGroupB,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupJ = paste(GANOVA_subData$Generation,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupK = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$FactorGroupB,GANOVA_subData$Generation,sep="")
# GANOVA_subData$FactorGroupL = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$FactorGroupB,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupM = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$Generation,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupN = paste(GANOVA_subData$FactorGroupB,GANOVA_subData$Generation,GANOVA_subData$SelectionDistribution,sep="")
# GANOVA_subData$FactorGroupO = paste(GANOVA_subData$FactorGroupA,GANOVA_subData$FactorGroupB,GANOVA_subData$Generation,GANOVA_subData$SelectionDistribution,sep="")
# 
# 
# FactorLevels = c(GANOVA_subData$FactorGroupA,GANOVA_subData$FactorGroupB,GANOVA_subData$FactorGroupC,
#                  GANOVA_subData$FactorGroupD,GANOVA_subData$FactorGroupE,GANOVA_subData$FactorGroupF,GANOVA_subData$FactorGroupG,
#                  GANOVA_subData$FactorGroupH,GANOVA_subData$FactorGroupI,GANOVA_subData$FactorGroupJ,GANOVA_subData$FactorGroupK,
#                  GANOVA_subData$FactorGroupL,GANOVA_subData$FactorGroupM,GANOVA_subData$FactorGroupN,GANOVA_subData$FactorGroupO)
# 
# Flowers_Visited = rep(GANOVA_subData$BLB_Flower_Visits,15)
# Fields_Visited = rep(GANOVA_subData$BLB_Fields_Visited,15)
# Memory_Points = rep(GANOVA_subData$BLB_mem,15)
# Loads = rep(GANOVA_subData$BLB_loads,15)
# Time_Harvesting = rep(GANOVA_subData$Time_Harvesting,15)
# Time_Scouting = rep(GANOVA_subData$Time_Scouting,15)
# Time_Returning = rep(GANOVA_subData$Time_Returning,15)
# Time_Total = rep(GANOVA_subData$Total_Time,15)
# 
# par(mfrow=c(2,4))
# GANOVA(cbind(as.factor(FactorLevels),Flowers_Visited),type="hist",center=FALSE,
#        main="# of BLB Flowers Visited",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Fields_Visited),type="hist",center=FALSE,
#        main="% of BLB Fields Visited",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Memory_Points),type="hist",center=FALSE,
#        main="% Memory Points in BLB",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Loads),type="hist",center=FALSE,
#        main="% BLB in Resource Loads",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Time_Harvesting),type="hist",center=FALSE,
#        main="% Time Harvesting",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Time_Scouting),type="hist",center=FALSE,
#        main="% Time Scouting",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Time_Returning),type="hist",center=FALSE,
#        main="% Time Returning",char=20)
# GANOVA(cbind(as.factor(FactorLevels),Time_Total),type="hist",center=FALSE,
#        main="Total Time",char=20)
# 
# boxplot(Flowers_Visited~FactorLevels,main="# of BLB Flowers Visited")
# boxplot(Fields_Visited~FactorLevels,main="% of BLB Fields Visited")
# boxplot(Memory_Points~FactorLevels,main="% Memory Points in BLB")
# boxplot(Loads~FactorLevels,main="% BLB in Resource Loads")
# boxplot(Time_Harvesting~FactorLevels,main="% Time Harvesting")
# boxplot(Time_Scouting~FactorLevels,main="% Time Scouting")
# boxplot(Time_Returning~FactorLevels, main="% Time Returning")
# boxplot(Time_Total~FactorLevels,main="Total Time")
# 
# 
# ################## GANOVAS for individual factors ###################
# Generation_Harvesting= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupC),Harvesting = GANOVA_subData$Time_Harvesting)
# Generation_Scouting= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupC),Scouting = GANOVA_subData$Time_Scouting)
# Generation_Returning= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupC),Returning = GANOVA_subData$Time_Returning)
# Generation_Total= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupC),TotalTime = GANOVA_subData$Total_Time)
# par(mfrow=c(2,2))
# GANOVA(Generation_Harvesting,type="hist",center=FALSE,main="% of time Harvesting",trtmt.names = c("Exploration","Random"))
# GANOVA(Generation_Scouting,type="hist",center=FALSE,main="% of time Scouting",trtmt.names = c("Exploration","Random"))
# GANOVA(Generation_Returning,type="hist",center=FALSE,main="% of time Returning",trtmt.names = c("Exploration","Random"))
# GANOVA(Generation_Total,type="hist",center=FALSE,main="Total Time Per Bout",trtmt.names = c("Exploration","Random"))
# 
# 
# Blueberry_TotalTime= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupB),TotalTime = GANOVA_subData$Total_Time)
# GANOVA(Blueberry_TotalTime,type="hist",center=FALSE,main="Total Time Per Bout",trtmt.names = c("high blueberry","low blueberry"))
# Blueberry_FlowersVisited= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupB),TotalTime = GANOVA_subData$BLB_Flower_Visits)
# GANOVA(Blueberry_FlowersVisited,type="hist",center=FALSE,main="# BLB Flowers Visited",trtmt.names = c("high blueberry","low blueberry"))
# 
# Resource_FieldsVisited= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupA),TotalTime = GANOVA_subData$BLB_Fields_Visited)
# GANOVA(Resource_FieldsVisited,type="hist",center=FALSE,main="% BLB Fields Visited",trtmt.names = c("high resource","low resource"))
# Resource_Scouting= data.frame(generation_method = as.factor(GANOVA_subData$FactorGroupA),TotalTime = GANOVA_subData$Time_Scouting)
# GANOVA(Resource_Scouting,type="hist",center=FALSE,main="% Time Scouting",trtmt.names = c("high resource","low resource"))
# 



All_The_Data[which(is.na(All_The_Data$N_BLB_Fields_Visited)),23]=0
#first make the data for Total time
N_BLB_Fields = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Fields = All_The_Data$N_BLB_Fields_Visited)
dataset = N_BLB_Fields
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(N_BLB_Fields,type="hist",center=FALSE,x.axis.label="Average NUMBER of blb fields visited",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments



#first make the data for Total time
N_BLB_Fields = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Fields = All_The_Data$N_BLB_Fields_Visited/All_The_Data$N_Fields_Visited)
dataset = N_BLB_Fields
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(N_BLB_Fields,type="hist",center=FALSE,x.axis.label="Percent of Field Visits to Blueberry",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments



#first make the data for Total time
N_BLB_Fields = data.frame(group = All_The_Data$ResourceBlbGenerationSelectionDistribution,Fields = All_The_Data$BLB_Landscape)
dataset = N_BLB_Fields
dataset <- dataset[order(dataset[, 1]), ]
data.split <- split(dataset[, 2], dataset[, 1])
treatments <- sapply(data.split, mean)     #this is the mean of each treatment group
#then do the plot
GANOVA(N_BLB_Fields,type="hist",center=FALSE,x.axis.label="Percent of Fields that are Blueberry",
       char=".")
points(treatments,rep(0,length(treatments)),pch=Shape+15,col=as.character(Color),cex=1)  #add the points on the x-axis
points(treatments,Level,pch=Shape+15,col=as.character(Color),cex=2)  #add the points on the different levels
abline(h=unique(Level),lty=2)   #add the horizontal lines for the treatments
