#The bee explores the landscape in a widening radius around the nest
#flower values at the endpoint of each flight segment are recorded
#movement is based on the Exploration_Raster version

### Parameters

bouts = 5                          #number of bouts to make

####################################################
DATAX=nx
DATAY=ny
memory_locations = c(0,0,0,0)



for(bee in 1:n_bees){
  print(paste("simulation ", sim_number, "exploring bee ", bee))
  
  #################################Do the thing####################################################
  ### Initial values and storage
  
  bouts_counter=1
  i = 2
  X = rep(NA,100*bouts)                 #fill X and Y with the initial point (nest coordinates)
  Y = rep(NA,100*bouts)
  X[1] = nx 
  Y[1] = ny      
  # Theta = numeric(100*bouts)           #store all of the turning angles
  # Times = numeric(100*bouts)             #store the inter-event intervals
  Counter = 0
  walks = 0                 #track how many exploring steps the bee has made
  
  max1 = 20             #initial radii of looping
  max2 = 30 
  # max_max1 = 120
  # max_max2 = 125 
  too_long_e = 100            #define how many steps to take   (this is different for exploration and foraging)
  D_i = 35   #increase in D_min and D_max
  e_i = 145   #increase in too_long
  radius = 10  #distance between inner and outer radii
  
  
  BeeID = c(3, nx, ny, runif(1,0,2*pi),0,1,0)
  #1 population status: 1=Harvester; 0=Scout; 2 = returning to nest; 3 = exploring
  #2,3 location: x & y coordinates of bee (nest is at (nx,ny))
  #4 angle: direction of travel (initial angle from memory direction)
  #5 amount of resource
  #6 flower type to search for; 1 for wildflower and 2 for blueberry
  #7 whether or not the bee has encountered flowers since leaving the nest (0=no,1=yes)
  
  # Theta[1] = BeeID[4]
  
  
  
  ### Make the bee move around! (in scouting mode only)
  
  while(bouts_counter<=bouts){   ## do things for bouts steps ##
    theta = BeeID[4]                #the angle the bee is going at
    
    
    if(BeeID[1] == 2){  ## what to do if the bee is done with an exploring bout
      new_y = ny
      new_x = nx                           #send bee back to nest
      # Times[i-1] = sqrt(BeeID[1]^2+BeeID[2]^2)/a    #calculate time to get back to nest
      BeeID[1] = 3                                #make the bee an explorer
      BeeID[4] = runif(1,0,2*pi)                 #new random angle 
      bouts_counter = bouts_counter+1            #the bee has completed another bout
      max1 = max1+D_i             #increase initial radii of looping
      max2 = max1+radius
      # max_max1 = 270*(bouts_exploration_counter-1)+120
      # max_max2 = 270*(bouts_exploration_counter-1)+130
      too_long_e = too_long_e+e_i
      
    }else if(BeeID[1]==3){## The bee is still exploring
      theta = BeeID[4]                             #the angle the bee travels at
      advect_time = rexp(1,mu)                     #the time to travel for
      # Times[i-1] = advect_time                       #store the time
      new_x = BeeID[2]+a*cos(theta)*advect_time    #the new location the bee travels to (velocity*time)
      new_y = BeeID[3]+a*sin(theta)*advect_time    #the new location the bee travels to (velocity*time)		
      
      ## Then decide what the next move will be
      if(walks >= too_long_e){  #the bee has been exploring long enough, so send it back to the nest 
        walks = 0                               #reset how many exploring steps the bee has made
        BeeID[1] = 2
      }else {  #the bee has not been exploring long enough, so keep going
        BeeID[4] = Turning3(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle 
        walks = walks+1				
      }
      
    }
    
    ## Store/update things
    X[i] = new_x            #bee's location at end of event
    Y[i] = new_y            #bee's location at end of event
    BeeID[2] = new_x		
    BeeID[3] = new_y
    # Theta[i] = BeeID[4]  
    
    # if(walks%%20 == 0){
    #   #if the number of walks is a multiple of 20, increase the looping radii
    #   max1 = min(max_max1,max1+100)
    #   max2 = min(max_max2,max2+100)
    # }
    # 
    i = i+1
  }
  
  
  ## Now calculate any potential memory locations 
  
  X_values = ceiling(X)
  Y_values = ceiling(Y)
  flower_values = numeric(length(X_values))
  for(index in 1:length(X_values)){
    flower_values[index] = embedded_landscape(X_values[index],Y_values[index])
  }
  bee_number = rep(bee,length(X_values))   #so that I can double check that all bees are getting added to memory_locations
  # bout_number = rep(bouts_counter,length(X_values))  #same for all bouts
  
  All = cbind(X,Y,bee_number,flower_values)
  
  ##this needs to give x and y values where there are flowers
  memory_locations =  rbind(memory_locations,All[which(All[,4]>0),])
  
  
  
  # ### Show a plot of where the bee went!
  # Xp = X[1:(i-1)]           #scale so that it works with the flower field image (heat map)
  # Yp = Y[1:(i-1)]
  # 
  # s = 1:(i-1)
  # 
  # plot(landscape_plotting,col=c("grey85","yellow1","lightcyan"),legend=FALSE,cex.axis=1.5)      #use this if plotting all the bees on one plot
  # 
  # segments(Xp[s],Yp[s],Xp[s+1],Yp[s+1])     #add lines for the path
  # # points(nx,ny,pch=20)   #make the nest obvious
  # # points(Xp[s],Yp[s],pch=".",col="red")
  
  
  
}




memory_cloud = unique(memory_locations[-1,])

#if any points are the nest, remove them
if(length(which(memory_cloud[,1]==nx))>0 & length(which(memory_cloud[,2]==ny))>0 ){
  nest_points =  intersect(which(memory_cloud[,1]==nx),which(memory_cloud[,2]==ny))
  memory_cloud = memory_cloud[-nest_points,]     #remove any points that are actually the nest	  		
}


Bees_Found_Flowers = unique(memory_cloud[,3])

n_bees_found_flowers = length(Bees_Found_Flowers)
BeesFoundFlowers[sim_number] = n_bees_found_flowers
