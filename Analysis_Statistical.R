#Get ALL of the data into one matrix
library(car)
setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-02-04")

#the plots will go through all of the selection methods/distributions, and average over the simulations and/or bees
landscapes = c("low_low","low_high", "high_low", "high_high")
generation_methods = c("random","exploration")
selection_methods = c("random","distance","type","energy")
distributions = matrix(c("Steep","Shallow","Linear","nonLinear","Linear","nonLinear"),nrow=3,ncol=2,byrow=TRUE)
  
n_blb_fields_visited = NULL
n_blb_fields = NULL

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
                          Successes=numeric(),Failures=numeric(),
                          N_BLB_Fields_Visited=numeric(),N_Fields_Visited=numeric(),stringsAsFactors=FALSE)


Data_matrix_index = 1

for(i in 1:length(landscapes)){
  for(j in 1:length(generation_methods)){
    for(k in 1:length(selection_methods)){
          # if(k == 5){    #set the working directory for the new type nonLinear distribution
          #   setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-04-14")
          # }else if(k == 7){    #set the working directory for the new energy nonLinear distribution
          #   setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-04-14")
          # }else{    #set the working directory for the other distributions
          #   setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-02-04")
          # }

          if(k == 1){ #the random selection 
          setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-02-04")          
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
            All_The_Data[Data_matrix_index,5] = Data_LandscapePercentResourceOnly[n,2]
              
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
            All_The_Data[Data_matrix_index,25] = visited_blb_fields
            n_blb_fields_visited = c(n_blb_fields_visited,visited_blb_fields)
            visited_fields = length(which(Data_TotalFlowerVisitLocations>0))
            All_The_Data[Data_matrix_index,26] = visited_fields
            visited_wf_fields = visited_fields - visited_blb_fields
            #count the total number of blb fields 
            total_blb_fields = Data_LandscapePercent[n,2]*n_fields
            n_blb_fields = c(n_blb_fields,total_blb_fields)
            total_wf_fields = Data_LandscapePercent[n,3]*n_fields
            total_fields = total_blb_fields + total_wf_fields
            #calculate the proportions and save the data in the right format
            All_The_Data[Data_matrix_index,8] = 100*visited_blb_fields/total_blb_fields  #the percent of blb fields visited
            All_The_Data[Data_matrix_index,9] = 100*visited_wf_fields/total_wf_fields
            All_The_Data[Data_matrix_index,10] = 100*visited_fields/total_fields
            All_The_Data[Data_matrix_index,23] = visited_blb_fields                     #number visited = successes
            All_The_Data[Data_matrix_index,24] = total_blb_fields - visited_blb_fields  #number not visited = failures
            
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
            if(k==3 & m==2){
              setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-04-14")
            }else if(k==4 & m==2){
              setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-04-14")
            }else{
              setwd("/home/macquees/Desktop/MemoryPaper/RemoteSimulationsCode/2020-02-04")
            }
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
              All_The_Data[Data_matrix_index,25] = visited_blb_fields
              n_blb_fields_visited = c(n_blb_fields_visited,visited_blb_fields)
              visited_fields = length(which(Data_TotalFlowerVisitLocations>0))
              All_The_Data[Data_matrix_index,26] = visited_fields
              visited_wf_fields = visited_fields - visited_blb_fields
              #count the total number of blb fields 
              total_blb_fields = Data_LandscapePercent[n,2]*n_fields
              n_blb_fields = c(n_blb_fields,total_blb_fields)
              total_wf_fields = Data_LandscapePercent[n,3]*n_fields
              total_fields = total_blb_fields + total_wf_fields
              #calculate the proportions and save the data in the right format
              All_The_Data[Data_matrix_index,8] = 100*visited_blb_fields/total_blb_fields  #the percent of blb in that simulation
              All_The_Data[Data_matrix_index,9] = 100*visited_wf_fields/total_wf_fields
              All_The_Data[Data_matrix_index,10] = 100*visited_fields/total_fields
              All_The_Data[Data_matrix_index,23] = visited_blb_fields                     #number visited = successes
              All_The_Data[Data_matrix_index,24] = total_blb_fields - visited_blb_fields  #number not visited = failures
              
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

All_The_Data$Landscape = factor(All_The_Data$Landscape,levels=unique(All_The_Data$Landscape))
All_The_Data$Generation = factor(All_The_Data$Generation,levels=unique(All_The_Data$Generation))
All_The_Data$Selection = factor(All_The_Data$Selection,levels=unique(All_The_Data$Selection))
All_The_Data$Distribution = factor(All_The_Data$Distribution,levels=unique(All_The_Data$Distribution))
All_The_Data$ResourceLevel = factor(All_The_Data$ResourceLevel,levels=unique(All_The_Data$ResourceLevel))
All_The_Data$BLBLevel = factor(All_The_Data$BLBLevel,levels=unique(All_The_Data$BLBLevel))
All_The_Data$SelectionDistribution = factor(All_The_Data$SelectionDistribution,levels=unique(All_The_Data$SelectionDistribution))

All_The_Data$BLB_mem_diff = All_The_Data$BLB_Landscape-All_The_Data$BLB_mem
All_The_Data$BLB_loads_diff = All_The_Data$BLB_Landscape-All_The_Data$BLB_loads

All_The_Data$Time_Total = All_The_Data$TotalTime_Scouting+All_The_Data$TotalTime_Harvesting+All_The_Data$TotalTime_Returning

attach(All_The_Data)


# #############################  MANOVA  ###################################
# manova_data = cbind(BLB_Fields_Visited,BLB_Flower_Visits,BLB_mem_diff,BLB_loads_diff,
#                     Time_Harvesting,Time_Scouting,Time_Total)
# # multivariate_model = lm(manova_data~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
# #                      ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
# #                      BLBLevel:Generation+BLBLevel:SelectionDistribution+
# #                      Generation:SelectionDistribution+
# #                      ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
# #                      ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
# #                      ResourceLevel:BLBLevel:Generation:SelectionDistribution)
# multivariate_model = lm(manova_data~ResourceLevel*BLBLevel*Generation*SelectionDistribution)
# # multivariate_model = lm(manova_data~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
# #                           ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
# #                           BLBLevel:Generation+BLBLevel:SelectionDistribution+
# #                           Generation:SelectionDistribution)
# multivariate_anova = manova(multivariate_model)
# summary(multivariate_anova)
# 
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
# #### BLB flower visits ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Flower_Visits)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Flower_Visits)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Flower_Visits)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Flower_Visits)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Flower_Visits)
# plot(All_The_Data$BLBLevel,All_The_Data$Flower_Visits)
# plot(All_The_Data$Generation,All_The_Data$Flower_Visits)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Flower_Visits)
# 
# #tests for equal variance 
# var.test(All_The_Data$Flower_Visits[ResourceLevel1],All_The_Data$Flower_Visits[ResourceLevel2])  #unequal
# var.test(All_The_Data$Flower_Visits[BLBLevel1],All_The_Data$Flower_Visits[BLBLevel2])  #equal
# var.test(All_The_Data$Flower_Visits[Generation1],All_The_Data$Flower_Visits[Generation2])  #equal
# bartlett.test(All_The_Data$Flower_Visits,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### BLB field visits ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Fields_Visited)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Fields_Visited)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Fields_Visited)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Fields_Visited)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Fields_Visited)
# plot(All_The_Data$BLBLevel,All_The_Data$Fields_Visited)
# plot(All_The_Data$Generation,All_The_Data$Fields_Visited)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Fields_Visited)
# 
# #tests for equal variance 
# var.test(All_The_Data$Fields_Visited[ResourceLevel1],All_The_Data$Fields_Visited[ResourceLevel2])  #unequal
# var.test(All_The_Data$Fields_Visited[BLBLevel1],All_The_Data$Fields_Visited[BLBLevel2])  #equal
# var.test(All_The_Data$Fields_Visited[Generation1],All_The_Data$Fields_Visited[Generation2])  #equal
# bartlett.test(All_The_Data$Fields_Visited,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### BLB mem difference #### 
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$BLB_mem_diff)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$BLB_mem_diff)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$BLB_mem_diff)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$BLB_mem_diff)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$BLB_mem_diff)
# plot(All_The_Data$BLBLevel,All_The_Data$BLB_mem_diff)
# plot(All_The_Data$Generation,All_The_Data$BLB_mem_diff)
# plot(All_The_Data$SelectionDistribution,All_The_Data$BLB_mem_diff)
# 
# #tests for equal variance 
# var.test(All_The_Data$BLB_mem_diff[ResourceLevel1],All_The_Data$BLB_mem_diff[ResourceLevel2])  #equal
# var.test(All_The_Data$BLB_mem_diff[BLBLevel1],All_The_Data$BLB_mem_diff[BLBLevel2])  #unequal
# var.test(All_The_Data$BLB_mem_diff[Generation1],All_The_Data$BLB_mem_diff[Generation2])  #equal
# bartlett.test(All_The_Data$BLB_mem_diff,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### BLB load difference ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$BLB_loads_diff)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$BLB_loads_diff)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$BLB_loads_diff)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$BLB_loads_diff)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$BLB_loads_diff)
# plot(All_The_Data$BLBLevel,All_The_Data$BLB_loads_diff)
# plot(All_The_Data$Generation,All_The_Data$BLB_loads_diff)
# plot(All_The_Data$SelectionDistribution,All_The_Data$BLB_loads_diff)
# 
# 
# #tests for equal variance 
# var.test(All_The_Data$BLB_loads_diff[ResourceLevel1],All_The_Data$BLB_loads_diff[ResourceLevel2])  #equal
# var.test(All_The_Data$BLB_loads_diff[BLBLevel1],All_The_Data$BLB_loads_diff[BLBLevel2])  #unequal
# var.test(All_The_Data$BLB_loads_diff[Generation1],All_The_Data$BLB_loads_diff[Generation2])  #equal
# bartlett.test(All_The_Data$BLB_loads_diff,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### %Time Harvesting ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Time_Harvesting)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Time_Harvesting)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Time_Harvesting)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Time_Harvesting)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Time_Harvesting)
# plot(All_The_Data$BLBLevel,All_The_Data$Time_Harvesting)
# plot(All_The_Data$Generation,All_The_Data$Time_Harvesting)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Time_Harvesting)
# 
# #tests for equal variance 
# var.test(All_The_Data$Time_Harvesting[ResourceLevel1],All_The_Data$Time_Harvesting[ResourceLevel2])  #unequal
# var.test(All_The_Data$Time_Harvesting[BLBLevel1],All_The_Data$Time_Harvesting[BLBLevel2])  #equal
# var.test(All_The_Data$Time_Harvesting[Generation1],All_The_Data$Time_Harvesting[Generation2])  #unequal
# bartlett.test(All_The_Data$Time_Harvesting,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### %Time Scouting ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Time_Scouting)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Time_Scouting)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Time_Scouting)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Time_Scouting)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Time_Scouting)
# plot(All_The_Data$BLBLevel,All_The_Data$Time_Scouting)
# plot(All_The_Data$Generation,All_The_Data$Time_Scouting)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Time_Scouting)
# 
# #tests for equal variance 
# var.test(All_The_Data$Time_Scouting[ResourceLevel1],All_The_Data$Time_Scouting[ResourceLevel2])  #unequal
# var.test(All_The_Data$Time_Scouting[BLBLevel1],All_The_Data$Time_Scouting[BLBLevel2])  #equal
# var.test(All_The_Data$Time_Scouting[Generation1],All_The_Data$Time_Scouting[Generation2])  #unequal
# bartlett.test(All_The_Data$Time_Scouting,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### %Time Returning ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Time_Returning)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Time_Returning)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Time_Returning)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Time_Returning)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Time_Returning)
# plot(All_The_Data$BLBLevel,All_The_Data$Time_Returning)
# plot(All_The_Data$Generation,All_The_Data$Time_Returning)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Time_Returning)
# 
# #tests for equal variance 
# var.test(All_The_Data$Time_Returning[ResourceLevel1],All_The_Data$Time_Returning[ResourceLevel2])  #unequal
# var.test(All_The_Data$Time_Returning[BLBLevel1],All_The_Data$Time_Returning[BLBLevel2])  #equal
# var.test(All_The_Data$Time_Returning[Generation1],All_The_Data$Time_Returning[Generation2])  #unequal
# bartlett.test(All_The_Data$Time_Returning,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# #### Total Time ####
# par(mfrow=c(2,2))
# plot(as.numeric(All_The_Data$ResourceLevel),All_The_Data$Time_Total)
# plot(as.numeric(All_The_Data$BLBLevel),All_The_Data$Time_Total)
# plot(as.numeric(All_The_Data$Generation),All_The_Data$Time_Total)
# plot(as.numeric(All_The_Data$SelectionDistribution),All_The_Data$Time_Total)
# 
# #boxplots
# plot(All_The_Data$ResourceLevel,All_The_Data$Time_Total)
# plot(All_The_Data$BLBLevel,All_The_Data$Time_Total)
# plot(All_The_Data$Generation,All_The_Data$Time_Total)
# plot(All_The_Data$SelectionDistribution,All_The_Data$Time_Total)
# 
# #tests for equal variance 
# var.test(All_The_Data$Time_Total[ResourceLevel1],All_The_Data$Time_Total[ResourceLevel2])  #equal
# var.test(All_The_Data$Time_Total[BLBLevel1],All_The_Data$Time_Total[BLBLevel2])  #unequal
# var.test(All_The_Data$Time_Total[Generation1],All_The_Data$Time_Total[Generation2])  #unequal
# bartlett.test(All_The_Data$Time_Total,All_The_Data$SelectionDistribution)  #unequal
# 
# 
# 
# ######################### Interaction Plots ###################################
# 
# par(mfrow=c(3,2))
# #BLB Flower Visits ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #BLB Flower visits ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB Flower visits ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB Flower visits ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$BLB_Flower_Visits,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# ########################
# 
# par(mfrow=c(3,2))
# #BLB Field Visits ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #BLB Field visits ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB Field visits ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB Field visits ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$BLB_Fields_Visited,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# ########################
# 
# par(mfrow=c(3,2))
# #BLB mem difference ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #BLB mem difference ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB mem difference ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB mem difference ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$BLB_mem_diff,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# 
# ########################
# 
# par(mfrow=c(3,2))
# #BLB load difference ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #BLB load difference ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB load difference ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #BLB load difference ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$BLB_loads_diff,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# ########################
# 
# par(mfrow=c(3,2))
# #%Time Harvesting ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #%Time_Harvesting ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Harvesting ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Harvesting ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$Time_Harvesting,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# 
# ########################
# 
# par(mfrow=c(3,2))
# #%Time_Scouting ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #%Time_Scouting ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Scouting ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Scouting~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$Time_Scouting,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# 
# ########################
# 
# par(mfrow=c(3,2))
# #%Time_Returning ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #%Time_Returning ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Returning ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #%Time_Returning ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$Time_Returning,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# 
# ########################
# 
# par(mfrow=c(3,2))
# #Time_Total ~ Resource level/BLB level and Generation method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$Generation,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=1:2,pch=1:2,lty=1:2)
# 
# #Time_Total ~ Resource level/BLB level and Selection method
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# interaction.plot(x.factor=All_The_Data$BLBLevel,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #Time_Total ~ Generation method and Selection method
# interaction.plot(x.factor=All_The_Data$Generation,trace.factor=All_The_Data$SelectionDistribution,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=1:7,lty=1:7,pch=1:7)
# 
# #Time_Total ~ Resource level and BLB level
# interaction.plot(x.factor=All_The_Data$ResourceLevel,trace.factor=All_The_Data$BLBLevel,
#                  response=All_The_Data$Time_Total,fun=mean,type="b",col=4:5,lty=4:5,pch=4:5)
# 
# 
# ########################


####################### Single Variable Model Fitting #################################

model_flowers = lm(BLB_Flower_Visits~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                    ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                    BLBLevel:Generation+BLBLevel:SelectionDistribution+
                    Generation:SelectionDistribution+
                    ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                    ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                    ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_flowers = Anova(model_flowers,type="II")
plot(model_flowers$residuals~model_flowers$fitted.values)

model_flowers_reg = lm(BLB_Flower_Visits~SelectionDistribution)
summary(model_flowers_reg)

model_fields = lm((N_BLB_Fields_Visited/N_Fields_Visited)~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
             ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
             BLBLevel:Generation+BLBLevel:SelectionDistribution+
             Generation:SelectionDistribution+
             ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
             ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
             ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_fields = Anova(model_fields,type="II")
plot(model_fields$residuals~model_fields$fitted.values)

model_fields_reg = lm(BLB_Fields_Visited~ResourceLevel)
summary(model_fields_reg)

model_mem = lm(BLB_mem_diff~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_mem = Anova(model_mem,type="II")
plot(model_mem$residuals~model_mem$fitted.values)

model_mem_reg = lm(BLB_mem_diff~BLBLevel+SelectionDistribution+BLBLevel:SelectionDistribution)
summary(model_mem_reg)

model_loads = lm(BLB_loads~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_loads = Anova(model_loads,type="II")
plot(model_loads$residuals~model_loads$fitted.values)

model_loads_reg = lm(BLB_loads~BLBLevel+SelectionDistribution)
summary(model_loads_reg) 

model_harvesting = lm(Time_Harvesting~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_harvesting = Anova(model_harvesting,type="II")
plot(model_harvesting$residuals~model_harvesting$fitted.values)

model_harvesting_reg = lm(Time_Harvesting~Generation+SelectionDistribution+Generation:SelectionDistribution)
summary(model_harvesting_reg)

model_scouting = lm(Time_Scouting~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_scouting = Anova(model_scouting,type="II")
plot(model_scouting$residuals~model_scouting$fitted.values)

model_returning = lm(Time_Returning~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_returning = Anova(model_returning,type="II")
plot(model_returning$residuals~model_returning$fitted.values)

model_totaltime = lm(Time_Total~ResourceLevel+BLBLevel+Generation+SelectionDistribution+
                     ResourceLevel:BLBLevel+ResourceLevel:Generation+ResourceLevel:SelectionDistribution+
                     BLBLevel:Generation+BLBLevel:SelectionDistribution+
                     Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation+ResourceLevel:BLBLevel:SelectionDistribution+
                     ResourceLevel:Generation:SelectionDistribution+BLBLevel:Generation:SelectionDistribution+
                     ResourceLevel:BLBLevel:Generation:SelectionDistribution)
anova_total_time = Anova(model_totaltime,type="II")
plot(model_totaltime$residuals~model_totaltime$fitted.values)


#store the anova results in a data frame
ANOVA_Results = data.frame(terms = c("ResourceLevel","BLBLevel","Generation","SelectionDistribution",
                            "ResourceLevel:BLBLevel","ResourceLevel:Generation","ResourceLevel:SelectionDistribution",
                            "BLBLevel:Generation","BLBLevel:SelectionDistribution",
                            "Generation:SelectionDistribution",
                            "ResourceLevel:BLBLevel:Generation","ResourceLevel:BLBLevel:SelectionDistribution",
                            "ResourceLevel:Generation:SelectionDistribution","BLBLevel:Generation:SelectionDistribution",
                            "ResourceLevel:BLBLevel:Generation:SelectionDistribution","Residual"),Flowers_Visited=numeric(16),
                           Fields_Visited=numeric(16), Memory_Points = numeric(16), Loads = numeric(16),
                           Time_Harvesting = numeric(16), Time_Scouting = numeric(16), Time_Returning = numeric(16), 
                           Total_Time=numeric(16))

ANOVA_Results[,3] = 100*anova_flowers$`Sum Sq`/sum(anova_flowers$`Sum Sq`)
ANOVA_Results[,2] = 100*anova_fields$`Sum Sq`/sum(anova_fields$`Sum Sq`)
ANOVA_Results[,4] = 100*anova_mem$`Sum Sq`/sum(anova_mem$`Sum Sq`)
ANOVA_Results[,5] = 100*anova_loads$`Sum Sq`/sum(anova_loads$`Sum Sq`)
ANOVA_Results[,6] = 100*anova_harvesting$`Sum Sq`/sum(anova_harvesting$`Sum Sq`)
ANOVA_Results[,7] = 100*anova_scouting$`Sum Sq`/sum(anova_scouting$`Sum Sq`)
ANOVA_Results[,8] = 100*anova_returning$`Sum Sq`/sum(anova_returning$`Sum Sq`)
ANOVA_Results[,9] = 100*anova_total_time$`Sum Sq`/sum(anova_total_time$`Sum Sq`)
colnames(ANOVA_Results) = c("terms", "Fields Visited", "Flowers Visited","Memory Points", "Resource Loads", "Harvesting", "Scouting", "Returning", "Total Time")
round(ANOVA_Results[,2:9],2)



####################### GANOVAs #################################
selection_methods = c("random","distance_Steep","distance_Shallow","type_Linear","type_nonLinear","energy_Linear","energy_nonLinear")

GANOVA_Harvesting_Generation = cbind(All_The_Data$Generation,All_The_Data$Time_Harvesting)
GANOVA(GANOVA_Harvesting_Generation,type="hist",center=FALSE,trtmt.names=generation_methods,char=c(1,2),
       char.col=1:2)
GANOVA_Harvesting_Selection = cbind(All_The_Data$SelectionDistribution,All_The_Data$Time_Harvesting)
GANOVA(GANOVA_Harvesting_Selection,type="hist",center=FALSE,trtmt.names=selection_methods,char=c(8,2,6,7,9,10,13),
       char.col=c("black","light blue","dark blue","pink","red","light green","dark green"))


GANOVA_Scouting_Generation = cbind(All_The_Data$Generation,All_The_Data$Time_Scouting)
GANOVA(GANOVA_Scouting_Generation,type="hist",center=FALSE,trtmt.names=generation_methods,char=c(1,2),
       char.col=1:2)
GANOVA_Scouting_Selection = cbind(All_The_Data$SelectionDistribution,All_The_Data$Time_Scouting)
GANOVA(GANOVA_Scouting_Selection,type="hist",center=FALSE,trtmt.names=selection_methods,char=c(8,2,6,7,9,10,13),
       char.col=c("black","light blue","dark blue","pink","red","light green","dark green"))
GANOVA_Scouting_Resource = cbind(All_The_Data$ResourceLevel,All_The_Data$Time_Scouting)
GANOVA(GANOVA_Scouting_Resource,type="hist",center=FALSE,trtmt.names=c("low","high"),char=1:2,
       char.col=1:2)

GANOVA_Returning_Generation = cbind(All_The_Data$Generation,All_The_Data$Time_Returning)
GANOVA(GANOVA_Returning_Generation,type="hist",center=FALSE,trtmt.names=generation_methods,char=c(1,2),
       char.col=1:2)
GANOVA_Returning_Selection = cbind(All_The_Data$SelectionDistribution,All_The_Data$Time_Returning)
GANOVA(GANOVA_Returning_Selection,type="hist",center=FALSE,trtmt.names=selection_methods,char=c(8,2,6,7,9,10,13),
       char.col=c("black","light blue","dark blue","pink","red","light green","dark green"))


GANOVA_Fields_Resource = cbind(All_The_Data$ResourceLevel,All_The_Data$Fields_Visited)
GANOVA(GANOVA_Fields_Resource,type="hist",center=FALSE,trtmt.names=generation_methods,char=c(1,2),
       char.col=1:2)
GANOVA_Returning_Selection = cbind(All_The_Data$SelectionDistribution,All_The_Data$Time_Returning)
GANOVA(GANOVA_Returning_Selection,type="hist",center=FALSE,trtmt.names=selection_methods,char=c(8,2,6,7,9,10,13),
       char.col=c("black","light blue","dark blue","pink","red","light green","dark green"))




####################### Regression Model Fitting #################################
### Uses only the factors found to be significant in the ANOVA variability table
par(mfrow=c(4,2))
boxplot(Successes~BLBLevel,ylab="Field Visits to BLB")
boxplot(BLB_Flower_Visits~SelectionDistribution,ylab="BLB Flowers Visited")
boxplot(BLB_mem_diff~BLBLevel+SelectionDistribution+BLBLevel:SelectionDistribution,ylab="Foraging Sites")
boxplot(BLB_loads~SelectionDistribution,ylab="Resource Loads")
boxplot(Time_Harvesting~Generation+SelectionDistribution+Generation:SelectionDistribution,ylab="% Harvesting")
boxplot(Time_Scouting~Generation+SelectionDistribution+Generation:SelectionDistribution,ylab="% Scouting")
boxplot(Time_Returning~Generation+SelectionDistribution+Generation:SelectionDistribution,ylab="% Returning")
boxplot(Time_Total~Generation+SelectionDistribution+Generation:SelectionDistribution,ylab="Total Time")

boxcox(BLB_Flower_Visits+1~SelectionDistribution)
boxcox(BLB_mem_diff+1~BLBLevel+SelectionDistribution+BLBLevel:SelectionDistribution)
boxcox(BLB_loads+1~SelectionDistribution)
boxcox(Time_Harvesting~Generation+SelectionDistribution+Generation:SelectionDistribution)
boxcox(Time_Scouting~Generation+SelectionDistribution+Generation:SelectionDistribution)
boxcox(Time_Returning~Generation+SelectionDistribution+Generation:SelectionDistribution)

model_fields_reg = glm(cbind(Successes,Failures)~BLBLevel,family=binomial)
summary(model_fields_reg)
plot(model_fields_reg$residuals~model_fields_reg$fitted.values,main="% BLB Fields Visited")
abline(h=0)

model_flowers_reg = glm((BLB_Flower_Visits)~SelectionDistribution,family=poisson)
summary(model_flowers_reg)
plot(model_flowers_reg$residuals~model_flowers_reg$fitted.values,main="Total BLB Flowers Visited")
abline(h=0)


model_mem_reg = lm(log(BLB_mem_diff)~BLBLevel+SelectionDistribution+BLBLevel:SelectionDistribution)
summary(model_mem_reg)
plot(model_mem_reg$residuals~model_mem_reg$fitted.values,main="% Memory spots in BLB - % BLB in Landscape")
abline(h=0)


model_loads_reg = lm(log(BLB_loads)~SelectionDistribution)
summary(model_loads_reg) 
plot(model_loads_reg$residuals~model_loads_reg$fitted.values,main="% BLB in resource loads")
abline(h=0)


model_harvesting_reg = glm((Time_Harvesting)~Generation+SelectionDistribution+Generation:SelectionDistribution,
                           family=Gamma(link=log))
summary(model_harvesting_reg)
plot(model_harvesting_reg$residuals~model_harvesting_reg$fitted.values,main="% Time Harvesting")
abline(h=0)


model_scouting_reg = glm(log(Time_Scouting)~Generation+SelectionDistribution+Generation:SelectionDistribution,
                           family=Gamma(link=log))
summary(model_scouting_reg)
plot(model_scouting_reg$residuals~model_scouting_reg$fitted.values,main="% Time Scouting")
abline(h=0)


model_returning_reg = glm(log(Time_Returning)~Generation+SelectionDistribution+Generation:SelectionDistribution,
                         family=Gamma(link=log))
summary(model_returning_reg)
plot(model_returning_reg$residuals~model_returning_reg$fitted.values,main="% Time Returning")
abline(h=0)


model_total_reg = glm(Time_Total~Generation+SelectionDistribution+Generation:SelectionDistribution,
                          family=Gamma(link=log))
summary(model_total_reg)
plot(model_total_reg$residuals~model_total_reg$fitted.values,main="Total Time Per Bout")
abline(h=0)



summary(model_fields_reg)
summary(model_flowers_reg)
summary(model_mem_reg)
summary(model_loads_reg) 
summary(model_harvesting_reg)
summary(model_scouting_reg)
summary(model_returning_reg)
summary(model_total_reg)
           




###############################################################################################################
##################### Plots of the Optimization Distributions #################################################
par(mfrow=c(1,3))

curve(dbeta(x,shape1=2,shape2=3),xaxt="n",xlab="distance from nest (m)", ylab = "probability density",cex.lab=1.5,ylim=c(0,5),col="blue",
      lty=2,lwd=2,cex.lab=1.5)
curve(dbeta(x,shape1=1.5,shape2=5),xaxt="n",xlab="distance from nest (m)",add=TRUE,col=2,lwd=2)
axis(1,labels=c(0,353,707,1060,1414),at=c(0,0.25,0.5,0.75,1),cex=2)
legend("topleft",legend=c("alpha=1.5, beta=5","alpha=2, beta=3"),
       text.col=c("red","blue"),lty=c(1,2),col=c("red","blue"),lwd=1.5,cex=1.5)

# curve(dbeta(x,shape1=1,shape2=.1),from=0,to=1,n=10001,xlab="fraction of wildflower at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5))   #type
curve(dbeta(x,shape1=1,shape2=.5),from=0,to=1,n=10001,xlab="fraction of wildflower at location", ylab = "probability density",cex.lab=1.5,
      ylim = c(0,5),col="2")   #type
# curve(dbeta(x,shape1=1,shape2=.9),from=0,to=1,n=10001,xlab="fraction of wildflower at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5),col="3",add=TRUE)   #type
curve(dbeta(x,shape1=2.5,shape2=1),from=0,to=1,n=10001,xlab="fraction of wildflower at location",
      col="4",add=TRUE,lty=2)   #type
# curve(dbeta(x,shape1=4,shape2=1),from=0,to=1,n=10001,xlab="fraction of wildflower at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5),col="5",add=TRUE)   #type
legend("topleft",legend=c("alpha=1, beta=0.5","alpha=2.5, beta=1"),
       text.col=c("red","blue"),lty=c(1,2),col=c("red","blue"))


# curve(dbeta(x,shape1=1,shape2=.1),from=0,to=1,n=10001,xlab="(normalized) net rate of energy gain at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5))   #type
curve(dbeta(x,shape1=1,shape2=.5),from=0,to=1,n=10001,xlab="(normalized) net rate of energy gain at location", ylab = "probability density",cex.lab=1.5,
      ylim = c(0,5),col="2")   #type
# curve(dbeta(x,shape1=1,shape2=.9),from=0,to=1,n=10001,xlab="(normalized) net rate of energy gain at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5),col="3",add=TRUE)   #type
curve(dbeta(x,shape1=2.5,shape2=1),from=0,to=1,n=10001,xlab="(normalized) net rate of energy gain at location",
      col="4",add=TRUE,lty=2)   #type
# curve(dbeta(x,shape1=4,shape2=1),from=0,to=1,n=10001,xlab="(normalized) net rate of energy gain at location", ylab = "density",cex.lab=1.5,
#       ylim = c(0,5),col="5",add=TRUE)   #type
# legend("topleft",legend=c("alpha=1, beta=0.1","alpha=1, beta=0.5","alpha=1, beta=0.9","alpha=2.5, beta=1","alpha=4, beta=1"),
#        text.col=1:5,lty=1,col=1:5)
legend("topleft",legend=c("alpha=1, beta=0.5","alpha=2.5, beta=1"),
       text.col=c("red","blue"),lty=c(1,2),col=c("red","blue"))

plot(1:10,(1:10)/55,xlab = "wildflower density or energy ranking", ylab = "Probability",pch=16,col="2",ylim=c(0,1),cex.lab=1.5,cex.axis=1.5,cex=1.5)
points(exp(1:10)/sum(exp(1:10)),pch=17,col="4",cex=1.5)
legend("topleft",legend=c("linear","exponential"),pch=c(16,17),col=c(2,4),cex=1.5)


### Plots of distributions for sensitivity analysis 
par(mfrow=c(4,4))
par(mar=c(2,1,2,1))
plot(1:10,rep(1/10,10),xlab = "Bin", ylab = "Probability",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
plot(1:10,c(1,1,1,1,1,5,5,5,5,5)/sum(c(1,1,2,2,3,3,4,4,5,5)),xlab = "Bin", ylab = "",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
plot(1:10,c(1,1,2,2,3,3,4,4,5,5)/sum(c(1,1,2,2,3,3,4,4,5,5)),xlab = "Bin", ylab = "",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
plot(1:10,(1:10)/55,xlab = "Bin", ylab = "",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
for(i in c(2,4,6,8)){
  for(j in c(2)){
    plot(1/(1+exp(-j*(1:10-i)))/sum(1/(1+exp(-j*(1:10-i)))),xlab = "Bin", ylab = "Probability",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
  }
}
for(i in 2:9){
plot(c(exp(1:i),rep(exp(i+1),10-i))/sum(c(exp(1:i),rep(exp(i+1),10-i))),xlab = "Bin", ylab = "Probability",pch=16,col="2",ylim=c(0,1),cex.lab=1,cex.axis=1,cex=1,type="b")
}

1/(1+exp(-1*(1:10-5)))

alphas = c(1,1.5,2,2.5,3,3.5)
betas = c(1,1,1.5,2,2.5,3,3.5)

alphas = 1:6
betas = 1:6

par(mfrow=c(6,6))
par(mar=c(2,1,2,1))
for(alp in 1:6){
  for(bet in 1:6){
    alpha = alphas[alp]
    beta = betas[bet]
    curve(dbeta(x,shape1=alpha,shape2=beta),from=0,to=1,n=10001,xlab="", ylab = "",cex.lab=.5,
          ylim = c(0,4),col="2",main=paste("alpha=",alpha,", beta=",beta))   #type
    
  }
}

