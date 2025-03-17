#Copied from Multiple_Bees_Raster_6b
#minimizing vector saving
#last updated: August 13 2019

too_long = 300       #define how many scouting steps without finding resource is too many (this is different for exploration and foraging)
Nest_Resource = c(1,1) #reset the nest resources at the beginning of each simulation (maybe move this a level higher) 


#### Storage 

all_X_harvested=matrix(NA,nrow=1,ncol=2)         #All X locations of all harvested flowers for all bees - separate columns to easily access only wf (1) or blb (2)
all_Y_harvested=matrix(NA,nrow=1,ncol=2)         #All Y locations of all harvested flowers for all bees
colnames(all_X_harvested) = c("wildflower","blueberry")
colnames(all_Y_harvested) = c("wildflower","blueberry")

Depleted = matrix(NA, nrow=n_bees_found_flowers, ncol=3)   #for each bee, store how many undepleted, depleted, non flowers she visited
colnames(Depleted) = c("undepleted","depleted","nonflower")

BLBPollen_Proportion = matrix(NA, nrow=n_bees_found_flowers, ncol=6)   #for each bee (row), store the percent of her resource load that is blueberry for each bout (column)
colnames(BLBPollen_Proportion) = c("bout1","bout2","bout3","bout4","bout5","bout6")

Bouts_Data = list()         #list to store matrices of complete/incomplete flower data for each bout
Bouts_Averages = matrix(NA, nrow=n_bees_found_flowers,ncol=4)   #columns are complete/incomplete (0/1); number of segments; amount of resources collected; amount of time spent 
colnames(Bouts_Averages) = c("incomplete", "n_segments","resource","time")

incomplete_bouts = 0   #count the number of incomplete bouts made by all bees
total_bouts = 0        #count the total number of bouts made by all bees (just to be safe)
total_collected = 0    #total amount of resource collected this simulation (by all bees combined)    

Sites = matrix(NA, nrow=n_bees_found_flowers, ncol=3)    #storage for (x,y) coordinates of memory point and flower type
colnames(Sites) = c("x","y","type")

Times = matrix(NA, nrow=n_bees_found_flowers, ncol=3)    #storage for amount of harvesting, scouting, and returning time (total of all 6 bouts)
colnames(Times) = c("harvesting","scouting","returning")

Segment_Times = NULL    #this will be a matrix with the amount of time in each segment and whether it is harvesting, scouting, or returning

if(plotting==TRUE){
  #to plot all bees together
  plot(landscape_plotting, xlab = "x", ylab = "y", xlim = c(0,x_meters), ylim = c(0,y_meters),asp=TRUE,col=c("grey85","yellow1","lightcyan"))
}

bee_index = 0

for(bee in Bees_Found_Flowers){
  bee_index = bee_index + 1
  
  print(paste("simulation ", sim_number, "foraging bee ", bee))
  # print(Sys.time())
  
  
  
  #################################Do the thing####################################################
  ### Initial values and storage
  
  X_harvested=matrix(NA,nrow=50000,ncol=2)         #All X locations of all harvested flowers for this bee (col1=wf; col2=blb)
  Y_harvested=matrix(NA,nrow=50000,ncol=2)         #All Y locations of all harvested flowers for this bee (col1=wf; col2=blb)
  harvested = c(0,0)                     #index for location vectors
  colnames(X_harvested) = c("wildflower","blueberry")
  colnames(Y_harvested) = c("wildflower","blueberry")
  
  if(plotting == TRUE){
    X_scouted = rep(NA,20000)      #X locations of scouting segment endpoints, to use in plotting
    Y_scouted = rep(NA,20000)      #X locations of scouting segment endpoints, to use in plotting
    scouted = 1                    #index for location vectors
  }
  
  bouts_data = matrix(NA, nrow=bouts,ncol=4)   #columns are complete/incomplete (0/1); number of segments; amount of resources collected; amount of time spent 
  colnames(bouts_data) = c("incomplete", "n_segments","resource","time")
  Time = 0               #amount of time this bout
  Segments = 0           #number of segments this bout
  incomplete = 0         #0 for incomplete bout, 1 for complete
  number_depleted = 0    #record how many times this bee visits a depleted flower
  number_undepleted = 0  #record how many undepleted flowers this bee visits
  number_notflower = 0   #record how many not flowers this bee visits (i.e. has diffused out of the flower patch)
  
  HarvestTime = 0        #amount of time spent in each mode (will be stored in Times matrix)
  ScoutTime = 0
  ReturningTime=0  
  
  bouts_counter=0
  i = 2
  
  current_x = nx                  #bee starts at the nest
  current_y = ny  
  
  if(plotting == TRUE){
    X_scouted[1] = nx               #the first move will be scouting, so record the nest endpoint of the first scouting segment
    Y_scouted[1] = ny
  }
  
  walks = 0                 #track how many scouting steps the bee has made --  maybe add this to DATA
  
  #subset the memory cloud
  memory_this_bee = memory_cloud[which(memory_cloud[,3]==bee),]  #all of the points that are for this bee
  
  #decide where to remember (x, y, flower type)  
  if(point_selection=="random"){
    where = memory_function_random()    #random selection from cloud of memory points        
  }else if(point_selection=="distance"){
    where = memory_function_distance()   #selection from cloud of memory points based on energy optimization
  }else if(point_selection=="type"){
    where = memory_function_type()
  }else if(point_selection=="energy"){
    where=memory_function_energy()
  }
  mx = where[1]                    #x and y coordinates of remembered location
  my = where[2]
  if(mx==nx & my == ny){ #keep the code from breaking if we manage to get the nest as the memory point by shifting it a tiny bit
    mx = mx + rnorm(1,0,1)
    my = my + rnorm(1,0,1)
  }
  Sites[bee_index,] = c(mx,my,where[4])     #store the memory location of this bee
  
  BeeID = c(0, nx, ny, Turning2(nx,ny,Memory(nx,ny,mx,my),nx,ny,max_dist),0,where[4],0,0,0)
  #1 population status: 1=Harvester; 0=Scout; 2 = returning to nest
  #2,3 location: x & y coordinates of bee (nest is at (nx,ny), where the bee starts)
  #4 angle: direction of travel (initial angle based on memory direction)
  #5 amount of resource
  #6 flower type to search for; 1 for wildflower and 2 for blueberry
  #7 whether or not the bee has encountered flowers since leaving the nest (0=no,1=yes)
  #8 amount of BLB resource
  #9 amount of wf resource
  
  
  ### Make the bee move around!
  
  while(bouts_counter<bouts){   ## do things for bouts steps ##
    
    
    if(BeeID[1]==0){  ## what to do if the bee is a scout ##
      
      ## The bee will advect first
      Segments = Segments + 1                      #the bee has done another segment
      theta = BeeID[4]                             #the angle the bee travels at
      advect_time = rexp(1,mu)                     #the time to travel for
      Time = Time + advect_time                    #total time so far
      ScoutTime= ScoutTime + advect_time
      new_x = current_x+a*cos(theta)*advect_time    #the new location the bee travels to (velocity*time)
      new_y = current_y+a*sin(theta)*advect_time    #the new location the bee travels to (velocity*time)		
      walks = walks+1	                              #increase number of scouting steps (this bout)
      
      Segment_Times = rbind(Segment_Times, c(advect_time,0))  #time scouting and 0 for scouting
      
      if(plotting==TRUE){
        scouted = scouted+1                           #increase scouting index for storage vector (all bouts)
      }
      
      memory = Memory(new_x,new_y,mx,my)           #the direction of the remembered location
      # memory_distance = sqrt((new_x-mx)^2+(new_y-my)^2)  #the distance from the memory spot
      # nest_distance = sqrt((new_x-nx)^2+(new_y-ny)^2)    #the distance from the nest
      Fl = embedded_landscape(new_x,new_y)       #the flower value
      
      #then the bee will decide what to do next 
      if(walks<=too_long){ #not scouting for too long, so check other stuff
        if(sqrt((new_x-nx)^2+(new_y-ny)^2)<=max_dist && BeeID[7]==1 && Fl>0){ #condition 1
          BeeID[1] = 1       #the bee becomes a harvester
        }else if(sqrt((new_x-nx)^2+(new_y-ny)^2)<=max_dist && BeeID[7]==1 && Fl==0){ #condition 2
          BeeID[1] = 0                                      #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle based on previous direction of travel
        }else if(sqrt((new_x-nx)^2+(new_y-ny)^2)<=max_dist && BeeID[7]==0 && sqrt((new_x-mx)^2+(new_y-my)^2)>=memory_radius){ #condition 3
          BeeID[1] = 0                                      #stay a scout
          BeeID[4] = Turning2(new_x,new_y,memory,nx,ny,max_dist)  #choose a new angle based on memory
        }else if(sqrt((new_x-nx)^2+(new_y-ny)^2)<=max_dist && BeeID[7]==0 && sqrt((new_x-mx)^2+(new_y-my)^2)<memory_radius && Fl==BeeID[6]){ #condition 4
          BeeID[1] = 1       #the bee becomes a harvester
        }else if(sqrt((new_x-nx)^2+(new_y-ny)^2)<=max_dist && BeeID[7]==0 && sqrt((new_x-mx)^2+(new_y-my)^2)<memory_radius && Fl!=BeeID[6]){ #condition 5
          BeeID[1] = 0                                      #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle based on previous direction of travel
        }else{ #condition 6
          BeeID[1] = 0                                       #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,memory,nx,ny,max_dist)   #choose a new angle (will be back towards nest)
          # print("too far away!")
        }
      }else{ #condition 7 (Scouting too long)
        BeeID[1] = 2                              #become a returner
        incomplete_bouts = incomplete_bouts+1     #this is an incomplete bout
        incomplete = 1
        # print("scouting too long!")
      }
      
      #update and store the results of the Scouting segment
      current_x = new_x              #the new x and y locations    
      current_y = new_y  
      
      if(plotting == TRUE){
        X_scouted[scouted] = current_x
        Y_scouted[scouted] = current_y
      }
      #end of Scouting 
      
      #      print("Scouted!")
      
    }else if(BeeID[1]==1){  ## what to do if the bee is a harvester ##
      Segments = Segments + 1                             #the bee has done (is doing?) another segment
      walks = 0                                           #reset the number of scouting steps the bee has made
      BeeID[7]=1                                          #bee has been a harvester now
      resource = c(0,0)                                   #the bee hasn't collected anything yet this harvesting segment
      
      #Check for depleted flowers of the same type at the current location to determine if the bee harvests here
      Fl = embedded_landscape(current_x,current_y)                       #flower value at location
      depletion_dist_temp = depletion_dists[Fl]
      distancex_indices = which(abs(current_x-all_X_harvested[,Fl])<depletion_dist_temp)   #just the points too close to the location in the x direction
      harvestedy = all_Y_harvested[distancex_indices,Fl]                                     #the corresponding y points
      distancey_indices=which(abs(current_y-harvestedy)<depletion_dist_temp)         #which of those points is also too close in the y direction
      depleted = length(distancey_indices)                                               #how many depleted spots the bee is too close to
      
      
      
      if(depleted==0){ #the flowers are undepleted
        resource[Fl] = resource[Fl] + resource_unit[Fl]                #add resource unit (to the correct type)
        X_harvested[harvested[Fl]+1,Fl] = current_x           #current_x the location the bee landed in
        Y_harvested[harvested[Fl]+1,Fl] = current_y           #harvested+1 is the number of flowers harvested including this one
        harvested[Fl] = harvested[Fl]+1                                    #the bee has harvested at 1 more flower
        number_undepleted = number_undepleted + 1                  #the bee visits one undepleted flower
      }else{ #the flowers are depleted
        number_depleted = number_depleted+1            #the bee has visited a depleted flower
      }
      
      #end of checking the location the bee arrived to the flower patch at
      
      #do the rest of the harvesting stuff
      harvest_time = sum(rexp(2,gamma1))               #how long the bee will stay a harvester for
      Time = Time+harvest_time                         #time so far in bout
      HarvestTime = HarvestTime+harvest_time
      steps = max(1,floor(harvest_time/(H[Fl])))   #how many steps the bee will make (totaltime/handlingtime) where handling time includes flying time
      
      Segment_Times = rbind(Segment_Times, c(harvest_time,1))  #time harvesting and 1 for harvesting

        for(j in 2:(steps+1)){   ## the bee diffuses/random walks for a while ##
        walk = rnorm(2,0,1)                                                              #random distances to travel in x and y directions
        grad = Edge_Checker(current_x, current_y)        #"gradient" of flower field at current location
        new_x = current_x+D1*walk[1] + grad[1]*deltat        #where the bee travels to        							
        new_y = current_y+D1*walk[2] + grad[2]*deltat        #where the bee travels to
        Fl = embedded_landscape(new_x,new_y)                           #flower value at the new location
        
        
        
        if(Fl>0){ #the bee is in flowers, so check for depletion
          depletion_dist_temp = depletion_dists[Fl]
          distancex_indices = which(abs(current_x-all_X_harvested[,Fl])<depletion_dist_temp)   #just the points too close to the location in the x direction
          harvestedy = all_Y_harvested[distancex_indices,Fl]                                     #the corresponding y points
          distancey_indices=which(abs(current_y-harvestedy)<depletion_dist_temp)         #which of those points is also too close in the y direction
          depleted = length(distancey_indices)                                               #how many depleted spots the bee is too close to
          
          if(depleted==0){ #the flowers are undepleted
            resource[Fl] = resource[Fl] + resource_unit[Fl]          #add resource unit to the right flower type
            X_harvested[harvested[Fl]+1,Fl] = new_x       #this is a location where the bee harvested
            Y_harvested[harvested[Fl]+1,Fl] = new_y
            harvested[Fl] = harvested[Fl]+1
            number_undepleted = number_undepleted + 1                  #the bee visits one undepleted flower
            current_x = new_x
            current_y = new_y
          }else{ #the flowers are depleted
            number_depleted = number_depleted + 1                  #the bee visits one undepleted flower
            current_x = new_x
            current_y = new_y
          }
          
        }else{ #the bee is not in flowers
          number_notflower = number_notflower+1    #this is not a flower
          current_x = new_x
          current_y = new_y
        }
        
        
      }	#end of harvesting movement loop 
      
      Nest_Resource = Nest_Resource + resource     #store the amount of resource collected in this segment
      BeeID[5] = BeeID[5]+sum(resource)         #total amount of resource collected on this bout. 
      BeeID[8] = BeeID[8] + resource[2]         #blb resource this bout
      BeeID[9] = BeeID[9] + resource[1]         #wf resource this bout
      
      if(BeeID[5] >= full){ #the bee is full
        BeeID[1] = 2                    #become a returner
      }else{ #the bee is not full
        BeeID[1] = 0                    #switch back to being a scout
        BeeID[4] = runif(1,0,2*pi)       #pick a new random angle to travel at as scout
        
      }
      
      #done doing the harvesting stuff
      #      print("Harvested!")
      
      
    }else if(BeeID[1] == 2){  ## what to do if the bee is a returner
      #count the bouts complete/incomplete
      returning_time = sqrt((current_x-nx)^2+(current_y-ny)^2)/a    #calculate time to get back to nest
      new_x = nx                                  #send bee back to nest
      new_y = ny                                  #send bee back to nest
      total_bouts = total_bouts+1
      total_collected = total_collected + BeeID[5]
      Segment_Times = rbind(Segment_Times, c(returning_time,2))  #time returning and 2 for returning
      
      walks = 0                                   #reset number of scouting steps bee has made
      ReturningTime=ReturningTime+returning_time
      Time = Time+returning_time
      bouts_data[bouts_counter+1,] = c(incomplete,Segments,BeeID[5],Time)   #store the data for this bout in incompleteness stuff
      BLBPollen_Proportion[bee_index,bouts_counter+1] = 100*(BeeID[8]/BeeID[5])       #blb resource/total resource
      
      # if(BeeID[5]==0){print(paste("Bee", bee, "no resource collected bout",bouts_counter))}
      
      BeeID[5] = 0                                #empty the resource
      BeeID[8] = 0
      BeeID[9] = 0
      BeeID[1] = 0                               #make the bee a scout
      BeeID[4] = Turning2(nx,ny,Memory(nx,ny,mx,my),nx,ny,max_dist)    #new angle based on memory
      bouts_counter = bouts_counter+1             #the bee has completed another bout
      BeeID[7] = 0                                #reset tracker for whether or not bee has found flowers since leaving nest
      Time = 0                                    #reset time in bout
      Segments = 1                                #reset segments in bout
      incomplete = 0                              #reset incompleteness
      current_x = new_x
      current_y = new_y
      
      
    }
    
    #Store/update things
    BeeID[2] = current_x		    #bee's location
    BeeID[3] = current_y		    #bee's location
    i = i+1
    
  } #end of bouts

  Bouts_Data[[bee_index]] = bouts_data    #store the incompleteness data for this bee in a list
  Bouts_Averages[bee_index,] = apply(bouts_data,2,mean)
  Depleted[bee_index,] = c(number_undepleted,number_depleted,number_notflower)      #store depleted/undepleted visits
  number_depleted = 0           #reset the things
  number_undepleted = 0
  number_notflower = 0
  
  Times[bee_index,] = c(HarvestTime,ScoutTime,ReturningTime)
  
  
  #Extract the harvesting coordinates for this bee to put in the matrix for all bees (used to check depletion while harvesting)
  X_blb_harvested = X_harvested[,2]        #just the blueberry (2nd column)
  X_blb_harvested = X_blb_harvested[!is.na(X_blb_harvested)]   #remove NAs
  Y_blb_harvested = Y_harvested[,2]        #just the blueberry  (2nd column)
  Y_blb_harvested = Y_blb_harvested[!is.na(Y_blb_harvested)]   #remove NAs
  X_wf_harvested = X_harvested[,1]        #just the wf  (1st column)
  X_wf_harvested = X_wf_harvested[!is.na(X_wf_harvested)]   #remove NAs
  Y_wf_harvested = Y_harvested[,1]        #just the wf (1st column)
  Y_wf_harvested = Y_wf_harvested[!is.na(Y_wf_harvested)]   #remove NAs
  
  
  #Add the above to the matrix of all flower points for checking depletion  (if there is anything in it)
  if(length(X_blb_harvested)>0){
    all_X_harvested = rbind(all_X_harvested,cbind(rep(NA,length(X_blb_harvested)),X_blb_harvested))   #put the blueberry vectors in the matrix (2nd column)
    all_Y_harvested = rbind(all_Y_harvested,cbind(rep(NA,length(Y_blb_harvested)),Y_blb_harvested))
  }
  
  if(length(X_wf_harvested)>0){
    all_X_harvested = rbind(all_X_harvested,cbind(X_wf_harvested,rep(NA,length(X_wf_harvested))))   #put the blueberry vectors in the matrix (2nd column)
    all_Y_harvested = rbind(all_Y_harvested,cbind(Y_wf_harvested,rep(NA,length(Y_wf_harvested))))
  }
  
  
  # print(paste("bee ",bee, " complete"))
  # print(Sys.time())
  
  
  if(plotting == TRUE){
    #Reduce the scouting coordinates for this bee (only used in plotting)
    X_scouted = X_scouted[!is.na(X_scouted)]
    Y_scouted = Y_scouted[!is.na(Y_scouted)]
    
    ### Show a plot of where the bee went!
    
    s = 1:(length(X_scouted)-1)
    segments(X_scouted[s],Y_scouted[s],X_scouted[s+1],Y_scouted[s+1], lty=1,col="grey")     #add lines for the path
    
    points(X_harvested,Y_harvested, col = "blue",pch=20,cex=.1)
    points(nx,ny,pch=20,cex=1.5)
    points(mx,my,col="red")
  }  
  
  
} #end of bumble bees loops







## Count the blueberry flower visits by all bees
all_blb_X_harvested = all_X_harvested[,2]                    #blueberry visits
all_blb_Y_harvested = all_Y_harvested[,2]
all_blb_X_harvested = all_blb_X_harvested[!is.na(all_blb_X_harvested)]
all_blb_Y_harvested = all_blb_Y_harvested[!is.na(all_blb_Y_harvested)]

all_wf_X_harvested = all_X_harvested[,1]                    #wf visits
all_wf_Y_harvested = all_Y_harvested[,1]
all_wf_X_harvested = all_wf_X_harvested[!is.na(all_wf_X_harvested)]
all_wf_Y_harvested = all_wf_Y_harvested[!is.na(all_wf_Y_harvested)]


PP_wf <- ppp(x=all_wf_X_harvested, y = all_wf_Y_harvested,window=W)    #the planar point pattern of the wf visited points
if(length(PP_wf$x)>0){ #if the bee visited any wildflowers, count it up
  Count_wildflower_visits = quadratcount(PP_wf, xbreaks=seq(0,2000,100), ybreaks=seq(0,2000,100))
  proportion4 = numeric(max(Count_wildflower_visits))
  #calculate the percent of wildflower fields with at least x flower visits
  for(i in 1:(max(Count_wildflower_visits)+1)){
    proportion4[i] = length(which(Count_wildflower_visits>(i-1)))/(n_fields)
  }
}else{ #no wf visits
  proportion4 = 0
  Count_wildflower_visits = 0
}

PP_blb <- ppp(x=all_blb_X_harvested, y = all_blb_Y_harvested,window=W)    #the planar point pattern of the blueberry visited points
if(length(PP_blb$x)>0){ #if the bee visited any blueberries, count it up
  Count_blueberry_visits = quadratcount(PP_blb, xbreaks=seq(0,2000,100), ybreaks=seq(0,2000,100))
  proportion3 = numeric(max(Count_blueberry_visits))
  #calculate the percent of blueberry fields with at least x flower visits
  for(i in 1:(max(Count_blueberry_visits)+1)){
    proportion3[i] = length(which(Count_blueberry_visits>(i-1)))/(n_fields)
  }
}else{ #no blb visits
  proportion3 = 0
  Count_blueberry_visits = 0
}

all_blb_wf_X_harvested = c(all_blb_X_harvested,all_wf_X_harvested)
all_blb_wf_Y_harvested = c(all_blb_Y_harvested,all_wf_Y_harvested)


## Count all of the flower visits by all bees
PP_all <- ppp(x=all_blb_wf_X_harvested, y = all_blb_wf_Y_harvested,window=W)    #the planar point pattern of all of the visited points
if(length(PP_all$x)>0){ #if the bee visited any flowers, count it up
  Count_visits = quadratcount(PP_all, xbreaks=seq(0,2000,100), ybreaks=seq(0,2000,100))
  proportion2 = numeric(max(Count_visits))
  #calculate the percent of fields (incl. wildflowers) with at least x flower visits
  for(i in 1:(max(Count_visits)+1)){
    proportion2[i] = length(which(Count_visits>(i-1)))/(n_fields)
  }
}else{ #no flower visits
  proportion2 = 0
  Count_visits = 0
}


#and calculate the total number of flower and blueberry flower visits 
total2 = sum(Count_visits)
total3 = sum(Count_blueberry_visits)
total4 = sum(Count_wildflower_visits)


#and calculate the highest number of visits to a field
max_wf = max(Count_wildflower_visits)
max_blb = max(Count_blueberry_visits)
max_all = max(Count_visits)

