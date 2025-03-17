
#last updated: August 23rd 2022
filepath = "~/SiteConstancy/Code"   #filepath to the main folder for this experiment

#Sys.time()
source(paste(filepath,"Functions_CPP.R",sep="/"))                    #source the functions file


plotting=FALSE                  #should we plot the tracks of the bees? 
new_landscape = TRUE           #should we generate a new random landscape?
plot_the_landscape = FALSE
point_generation = "exploration"     #options are "random" or "exploration" for how the cloud of potential memory points is generated
point_selection="random"        #options are "random" "distance" "type" "energy" for how the memory point is chosen from the (subset) cloud
prob_distr = c(.01,.01,.01,.01,.01,.05,.05,.15,.15,.55)   #for the distribution used in point selection (type and energy)

nsim = 20
n_bees = 50

date = "SimResults"           #fill in with today's date (year_month_day) and create this folder in Data Files


##### Set all of the parameters and things that hold for all simulations ####

gamma1 = .03                  #rate of becoming scout (switching to scouting from harvesting) - controls length of time harvesting
mu = 1                        #rate of switching angle while scouting  (lower rate = higher times more likely) - controls amount of time flying in a particular direction
a =   4.5                     #advection speed (m/s) observed in a paper
D =   .01                     #diffusion coefficient (variance)
full = 80                     #microliters of nectar for a full load
resource_unit = .8*c(1,1)           #microliters of resource collected per flower (wf, blb) - conservative estimate
#resource_unit = .8*c(2.27,6.145)           #microliters of resource collected per flower (wf, blb)
H = c(5,7.99)                  #handling times (wf,blb) (Cresswell, Rodrigues-Saona2011)
deltat = 0.1                  #distance step size parameter for the random walk/diffusion 
D1 = sqrt(2*D*deltat)         #
bouts = 6                     #number of bouts to make (6=1 single day, from two different papers)
depletion_dists = c(0.0135,0.0749)           #1=wildflower; 2=blueberry
max_dist = 2000                     #max distance the bee will go from the nest (meters)
memory_radius = 5              #how close bee must get to memory location to start harvesting
kappa = 30   #variance of von Mises distribution 

nx = 1000                     #coordinates of nest
ny = 1000
p_e = 1/3
p_w = 1/3
p_b = 1/3


#parameters used in calculating rate of energy return 
n = 2.5         #size of square to use to calculate amount of resource types at memory spot = 2nx2n (meters)
alpha = 0.1     #distance between grid points used to calculate amount of resource types at memory spot  (meters)
full_square_wf = ((2*n)/depletion_dists[1])^2     #total number of flowers in a full wf square

full_square_blb = ((2*n)/depletion_dists[2])^2    #total number of flowers in a full blb square     
# R_b = .8*6.145     #microliters of nectar per blueberry flower (see BlueStew - average of a couple of sources)
# R_w = .8*2.27      #microliters of nectar per wild flower  (average of all non crop/non-oilseed rape species in BlueStew)
R_b = .8*1    #microliters of nectar per blueberry flower (conservative estimate)
R_w = .8*1      #microliters of nectar per wild flower  (conservative estimate)
Lambda_b = 7.23*10       #units of metabolic energy per microliter of resource   (calculated with molar mass, etc.)
Lambda_w = 3.99*10       #units of metabolic energy per microliter of resource   (calculated with molar mass, etc.)
C = full        #microliters of nectar a bee can carry (80 microliters from Cresswell)
S = a           #speed of travel (a=4.5m/s)
H_b = H[2]          #blueberry handling time (s)  (also time step size in harvesting)
H_w = H[1]          #wildflower handling time (s)   (also time step size in harvesting)
M_fly = 0.33         #mean rate of expenditure while flying (travelling) (J/s) (Cartar/Heinrich)
M_provision = .235   #mean rate of expenditure while provisioning (harvesting) (J/s)  (Cartar/Heinrich)

##############




########### Storage  #############

PROPORTION_all = numeric(nsim)
PROPORTION_blue = numeric(nsim)
PROPORTION_wf = numeric(nsim)
TOTAL_all = numeric(nsim)        #storage for the total number of flower visits
TOTAL_blue = numeric(nsim)
TOTAL_wf = numeric(nsim)


MAX = matrix(NA, nrow=nsim, ncol=3)     #maxiumum number of visits to any one field
colnames(MAX) = c("all","blueberry","wildflower")

NEST_RESOURCE = matrix(NA, nrow=nsim, ncol=2)
colnames(NEST_RESOURCE) = c("wildflower","blueberry")

BeesFoundFlowers = numeric(nsim)   #storage for the number of bees that found flowers in the simulation

LANDSCAPE_PROPORTIONS = matrix(NA, nrow=nsim,ncol=3)     #the percent of blueberry, wf, no resource cells in the landscape
LANDSCAPE_PROPORTIONS_Resource = matrix(NA, nrow=nsim,ncol=2) 

###################################################################################################
############################# Simulations Begin Here ##############################################

for(sim_number in 1:nsim){
  if(new_landscape==TRUE){
    source(paste(filepath,"Landscape_Raster_1.R",sep="/"))    #source the right landscape file
  }


  
  #first have the bees explore, and save the memory locations for this simulation
  if(point_generation == "random"){
   source(paste(filepath,"PointGeneration_random.R",sep="/"))
    # filename2b = paste("Memory_Locations","Memory",memory_typea,"Simulation",sim_number,sep="_")
    # write.csv(memory_locations,file=paste("/home/macquees/Documents/Project/Code/Data Files",date,filename2b,sep="/")) 
  }else{  #point_generation is "exploration"
    source(paste(filepath,"PointGeneration_exploration.R",sep="/"))
  }

  
  #then have the bees do their foraging thing and save a bunch of stuff
  source(paste(filepath,"Multiple_Bees_Raster_6c.R",sep="/"))
  
  # #the number of unique bees visiting each field 
  # filename3 = paste("Total_Bees_on_Fields","Memory",memory_type,"Simulation",sim_number,sep="_")
  # dput(Total_Bees_on_Fields,file=paste("/home/macquees/Documents/Project/Code/Data Files",date,filename3,sep="/")) 
  

  #the total number of visits to each field (quadratcount)
  filename4 = paste("TotalFlowerVisitLocation",point_generation,point_selection,"Simulation",sim_number,sep="_")
  dput(Count_visits,file=paste(filepath,date,filename4,sep="/"))    

  #the total number of visits to each blueberry field  (quadratcount)
  filename4a = paste("BLBFlowerVisitLocations",point_generation,point_selection,"Simulation",sim_number,sep="_")
  dput(Count_blueberry_visits,file=paste(filepath,date,filename4a,sep="/"))    
  
  #the bout data for each bee (complete/incomplete; number of segments; amount of resources collected; amount of time spent)
  filename4b = paste("BoutsData",point_generation,point_selection,"Simulation",sim_number,sep="_")
  dput(Bouts_Data,file=paste(filepath,date,filename4b,sep="/"))    

  #the bout average data for each bee
  filename4c = paste("BoutsAverages",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(Bouts_Averages,file=paste(filepath,date,filename4c,sep="/"))    
  

  # #the percent of fields visited by at least x (different) bees
  # filename5 = paste("Proportion_Bees_on_Fields","Memory",memory_type,"Simulation",sim_number,sep="_")
  # write.csv(proportion1,file=paste("/home/macquees/Documents/Project/Code/Data Files",date,filename5,sep="/"))
  
  #the percent of fields with at least x flower visits
  filename6 = paste("ProportionxFieldVisits",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(proportion2,file=paste(filepath,date,filename6,sep="/"))

  #the percent of blueberry fields with at least x flower visits
  filename6a = paste("ProportionxBLBFieldVisits",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(proportion3,file=paste(filepath,date,filename6a,sep="/"))
  
  #the total, undepleted, depleted flower visits
  filename6b = paste("UndepletedDepletedVisits",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(Depleted,file=paste(filepath,date,filename6b,sep="/"))

  #the memory site information
  filename6c = paste("MemoryLocations",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(Sites,file=paste(filepath,date,filename6c,sep="/"))
  
  #the time in each movement mode
  filename6d = paste("HSRTimes",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(Times,file=paste(filepath,date,filename6d,sep="/"))
  
  #the percent of blueberry pollen in each pollen load
  filename6e = paste("BLBPollenLoadPercent",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(BLBPollen_Proportion,file=paste(filepath,date,filename6e,sep="/"))
  
  #the time of each segment
  filename6d = paste("SegmentTimes",point_generation,point_selection,"Simulation",sim_number,sep="_")
  write.csv(Segment_Times,file=paste(filepath,date,filename6d,sep="/"))
  
  PROPORTION_all[sim_number] = proportion2[1]   #the percent of fields where at least one flower was visited
  PROPORTION_blue[sim_number] = proportion3[1]   #percent of blueberry fields where at least one flower was visited
  PROPORTION_wf[sim_number] = proportion4[1]   #percent of wf fields where at least one flower was visited
  
  TOTAL_all[sim_number] = total2    #the total number of flower visits
  TOTAL_blue[sim_number] = total3   #the total number of blueberry flower visits
  TOTAL_wf[sim_number] = total4     #the total number of wildflower visits
  
  MAX[sim_number,] = c(max_all,max_blb,max_wf)    #the maximum number of visits to a field
  
  #Save the nest resource for this simulation
  NEST_RESOURCE[sim_number,] = Nest_Resource
  
}

############################# Simulations End Here ##############################################
###################################################################################################

filename0 = paste("LandscapePercent",point_generation,point_selection,sep="_")
colnames(LANDSCAPE_PROPORTIONS) = c("blueberry","wildflower","nothing")
write.csv(LANDSCAPE_PROPORTIONS,file=paste(filepath,date,filename0,sep="/"))    

filename0a = paste("LandscapePercentResourceOnly",point_generation,point_selection,sep="_")
colnames(LANDSCAPE_PROPORTIONS_Resource) = c("blueberry","wildflower")
write.csv(LANDSCAPE_PROPORTIONS_Resource,file=paste(filepath,date,filename0a,sep="/"))    


filename8 = paste("ProportionFieldsVisited",point_generation,point_selection,sep="_")
write.csv(cbind(PROPORTION_all,PROPORTION_blue,PROPORTION_wf),file=paste(filepath,date,filename8,sep="/"))

filename8a = paste("TotalFlowersVisited",point_generation,point_selection,sep="_")
write.csv(cbind(TOTAL_all,TOTAL_blue,TOTAL_wf),file=paste(filepath,date,filename8a,sep="/"))

filename8b = paste("MaxVisitsToField",point_generation,point_selection,sep="_")
write.csv(MAX,file=paste(filepath,date,filename8b,sep="/"))


filename10 = paste("NestStores",point_generation,point_selection,sep="_")
write.csv(NEST_RESOURCE,file=paste(filepath,date,filename10,sep="/"))

filename11 = paste("BeesFoundFlowers",point_generation,point_selection,sep="_")
write.csv(BeesFoundFlowers,file=paste(filepath,date,filename11,sep="/"))    

# #make the plots from the last simulation for poster/presentation purposes
# 
# #determine the color scheme for the heatmaps
# breaks = c(0,seq(0.01,1.01,.01))
# number = seq(0,100,1)
# colors = paste("grey",number,sep="")
# 
# 
# # image.plot(t(apply(Total_Bees_on_Fields/max(Total_Bees_on_Fields), 2, rev)), col = rev(colors), breaks = breaks,
# #                   xlab = "", ylab = "", main = "Intensity of Bees Visiting Each Field",axes=FALSE)
# 
# image.plot(t(apply(Count_visits/max(Count_visits), 2, rev)), col = rev(colors), breaks = breaks,
#                   xlab = "", ylab = "", main = "Intensity of Flowers Visited in Each Field",axes=FALSE)
# points(Sites[,1]/2000,Sites[,2]/2000,pch=20,col=2,cex=.5)    #add the memory points
# 
# image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
#                   xlab = "", ylab = "", main = "Intensity of Flowers Visited in Each Blueberry Field")
# points(Sites[,1]/2000,Sites[,2]/2000,pch=20,col=2,cex=.5)    #add the memory points
# points(nx/2000,ny/2000,pch=20,col=4)
# 
# # # plot(proportion1*100,xlab="number of bees", ylab = "percent of fields visited",pch = 20,ylim = c(0,40),cex=2)
# # plot(proportion2*100,xlab="number of visits", ylab = "percent of fields",pch = 20,ylim = c(0,40))
# # plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry fields",pch = 20,ylim = c(0,40))
# # 
# 
# ## Count all of the flower visits by all bees in 10m squares
# PP_all_10m <- ppp(x=all_blb_wf_X_harvested, y = all_blb_wf_Y_harvested,window=W)    #the planar point pattern of all of the visited points
# if(length(PP_all$x)>0){ #if the bee visited any flowers, count it up
#   Count_visits_10m = quadratcount(PP_all_10m, xbreaks=seq(0,2000,10), ybreaks=seq(0,2000,10))
# }
# image.plot(t(apply(Count_visits_10m/max(Count_visits_10m), 2, rev)), col = rev(colors), breaks = breaks,
#            xlab = "", ylab = "", main = "Intensity of Flowers Visited in Each Field",axes=FALSE)
# points(Sites[,1]/2000,Sites[,2]/2000,pch=20,col=2,cex=.5)    #add the memory points
# points(nx/2000,ny/2000,pch=20,col=4)
# 
# 
# # ## Count all of the flower visits by all bees in 1m squares
# # PP_all_1m <- ppp(x=all_blb_wf_X_harvested, y = all_blb_wf_Y_harvested,window=W)    #the planar point pattern of all of the visited points
# # if(length(PP_all$x)>0){ #if the bee visited any flowers, count it up
# #   Count_visits_1m = quadratcount(PP_all_1m, xbreaks=seq(0,2000,1), ybreaks=seq(0,2000,1))
# # }
# # image.plot(t(apply(Count_visits_1m/max(Count_visits_1m), 2, rev)), col = rev(colors), breaks = breaks,
# #            xlab = "", ylab = "", main = "Intensity of Flowers Visited in Each Field",axes=FALSE)
# # points(Sites[,1]/2000,Sites[,2]/2000,pch=20,col=2,cex=.5)    #add the memory points
# # points(nx/2000,ny/2000,pch=20,col=4)
# 
# 
# #plot the memory points
# landscape_skeleton <- raster(ncol = n_fields_x,nrow = n_fields_y,xmn=0,xmx=x_meters,ymn=0,ymx=y_meters)   #creates the "skeleton"
# values(landscape_skeleton) = cell_values
# landscape_plotting = flip(landscape_skeleton,2)  #landscape raster to use for plotting (flipped to put x[1,1] at bottom left)
# plot(landscape_plotting, xlab = "x", ylab = "y", xlim = c(0,x_meters), ylim = c(0,y_meters),asp=TRUE,col=c("grey85","yellow1","lightcyan"),
#      main=paste(point_generation,point_selection,sep=", "))
# points(Sites[,1],Sites[,2],pch=20,cex=.5,col=2)
# points(nx,ny,pch=20,col="blue")
# 
# # #plot a sample memory cloud
# # landscape_skeleton <- raster(ncol = n_fields_x,nrow = n_fields_y,xmn=0,xmx=x_meters,ymn=0,ymx=y_meters)   #creates the "skeleton"
# # values(landscape_skeleton) = cell_values
# # landscape_plotting = flip(landscape_skeleton,2)  #landscape raster to use for plotting (flipped to put x[1,1] at bottom left)
# # plot(landscape_plotting, xlab = "x", ylab = "y", xlim = c(0,x_meters), ylim = c(0,y_meters),asp=TRUE,col=c("grey85","yellow1","lightcyan"),
# #      main="Potential Memory Points, Randomly Generated")
# # points(memory_this_bee[,1],memory_this_bee[,2],pch=20,cex=.5,col="purple")
# # points(nx,ny,pch=20,col="blue")
# 
# 
# Sys.time()
