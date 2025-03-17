
# install.packages("circular")
# install.packages("spatstat")
# install.packages("fields")
# install.packages("car")
# install.packages("raster")
# install.packages("Rcpp") #used to compile c++ code
# install.packages("MPV")

library(fields)
library(circular)
library(spatstat)
library(car)
library(raster)
library(Rcpp)
library(MPV)

#function findDepleted
# sourceCpp("/home/macquees/Documents/Project/Code/Models/depletedBee.cpp")    #this is the c++ code that Riley wrote


Turning1 = function(x,y,theta,n1=0,n2=0,max=11){ ## the turning angle distribution ##
  t_x = x-n1            #transform so that (n1,n2) = (0,0)
  t_y = y-n2
  dist = sqrt(t_x^2+t_y^2)
  angle = rvonmises(1,circular(theta),30,control.circular=list(units="radians"))             
  #von Mises, centered at the angle the bee is traveling at, to make small angle changes more likely
  return(angle)
}


Turning2 = function(x,y,theta,n1=0,n2=0,max=1000){ ## the turning angle distribution ##
  #(x,y) is location of bee; (n1,n2) is location of nest
  #theta is direction of travel
	t_x = x-n1            #transform so that (n1,n2) = (0,0)
	t_y = y-n2
	dist = sqrt(t_x^2+t_y^2)
	if(dist <= max){
		angle = rvonmises(1,circular(theta),30,control.circular=list(units="radians"))             
					#von Mises, centered at the angle the bee is traveling at, to make small angle changes more likely
		      #3rd entry controls how "focused" the direction is

	}else if(t_x<0 && t_y<0){
		angle = runif(1,acos(-t_x/dist)-pi/16, acos(-t_x/dist)+pi/16)
	}else if(t_x>0 && t_y<0){
		angle = runif(1,pi - acos(t_x/dist)-pi/16, pi - acos(t_x/dist)+pi/16)
	}else if(t_x>0 && t_y>0){
		angle = runif(1,pi + acos(t_x/dist)-pi/16,pi + acos(t_x/dist)+pi/16)
	}else if(t_x<0 && t_y>0){
		angle = runif(1,2*pi - acos(-t_x/dist)-pi/16,2*pi - acos(-t_x/dist)+pi/16)
	}	
	return(angle)
}

Turning3 = function(x,y,theta,n1=0,n2=0,max=11){ ## the turning angle distribution for exploration ##
  t_x = x-n1            #transform so that (n1,n2) = (0,0)
  t_y = y-n2
  dist = sqrt(t_x^2+t_y^2)
  if(dist <=max1){    
    #not very far away from nest, so continue in initial direction of travel
    angle = rvonmises(1,circular(theta),30,control.circular=list(units="radians"))             
    #von Mises, centered at the angle the bee is traveling at, to make small angle changes more likely
    
  }else if(max1 <= dist && dist <= max2){   
    #some distance away from the nest, but not too far away, so travel perpendicularly to nest direction
    
    #calculate direction to nest
    if(t_x<0 && t_y<0){
      angle1 = acos(-t_x/dist)
    }else if(t_x>0 && t_y<0){
      angle1 = pi - acos(t_x/dist)
    }else if(t_x>0 && t_y>0){
      angle1 = pi + acos(t_x/dist)
    }else if(t_x<0 && t_y>0){
      angle1 = 2*pi - acos(-t_x/dist)
    }
    angle2 = sample(x = c(angle1+(pi/2),angle1,angle1+pi), size = 1, prob = c(.6,.35,.05))    #perpendicular to nest direction; continue in same direction; towards nest
    angle = rvonmises(1,circular(angle2),30,control.circular=list(units="radians"))   #use vonM for randomness
    
  }else{
    #too far away, so go back towards the nest
    if(t_x<0 && t_y<0){
      angle1 = acos(-t_x/dist)
    }else if(t_x>0 && t_y<0){
      angle1 = pi - acos(t_x/dist)
    }else if(t_x>0 && t_y>0){
      angle1 = pi + acos(t_x/dist)
    }else if(t_x<0 && t_y>0){
      angle1 = 2*pi - acos(-t_x/dist)
    }
    angle2 = sample(x=c(angle1,angle1+pi),size=1,prob=c(.7,.3))  #back towards nest; continue in same direction
    angle = rvonmises(1,circular(angle2),30,control.circular=list(units="radians"))   #use vonM for randomness
    
  }	
  return(angle)
}


angle_switching_rate = function(x,y){
	if(flowers(x,y)> .5){    #when more flowers
		1.5    #a higher rate makes smaller times more likely
	}else{    #when fewer flowers
		1     #a lower rate makes longer times more likely
		}
}


Initial = function(Interval){
	a = Interval[1]
	b = Interval[2]
	angle = runif(1,a,b)
}


Memory = function(x,y,m1,m2){ ## the memory angle calculator 
	#(x,y) is location of bee
	#(m1,m2) is memory
	t_1 = m1-x    #memory location transformed so that bee is at (0,0)
	t_2 = m2-y
	h = sqrt(t_1^2+t_2^2)
	
	if(t_1>=0 && t_2>=0){
		angle = acos(t_1/h)
	}else if(t_1<0 && t_2>=0){
		angle = pi - acos(-t_1/h)
	}else if(t_1<=0 && t_2<0){
		angle = pi + acos(-t_1/h)
	}else if(t_1>0 && t_2<=0){
		angle = 3*pi/2 + acos(-t_2/h)
		}	
	return(angle)
}


# sides = matrix(c(1,0,-1,0,0,1,0,-1,2,0,-2,0,0,2,0,-2),nrow=8,byrow=TRUE)
sides = matrix(c(1,0,-1,0,0,1,0,-1),nrow=4,byrow=TRUE)

Edge_Checker = function(x,y){
  #checks the four direct neighbor cells to see if any of them are non-flower
  #uses random order of checking, and uses the first found non-flower neighbor cell for gradient direction 
  #to avoid bias in gradient direction at corners
  
  #x and y are grid cell values of bee's current location
  
  order = sample(1:4,4,replace=FALSE)
  b=c(0,0)
  i = 1
  while(i<=4 && b==c(0,0)){
    side = sides[order[i],]
    cell_to_check = c(x,y)+side
    if(embedded_landscape(cell_to_check[1],cell_to_check[2])==0){
      b = -side*4
    }
    i = i+1
  }
  return(b)
}


# Edge_Checker2 = function(xy_coords){
#   #checks the four direct neighbor cells to see if any of them are non-flower
#   #uses random order of checking, and uses the first found non-flower neighbor cell for gradient direction 
#   #to avoid bias in gradient direction at corners
#   
#   #x and y are grid cell values of bee's current location
#   
#   order = sample(1:4,4,replace=FALSE)
#   b=c(0,0)
#   i = 1
#   while(i<=4 && b==c(0,0)){
#     side = sides[order[i],]
#     cell_to_check = c(x,y)+side
#     if(landscape_values[cell_to_check[1],cell_to_check[2]]==0){
#       b = -side*4
#     }
#     i = i+1
#   }
#   return(b)
# }
# 
# 
# xy_to_matrix = function(x_coord,y_coord){
#   if(x_coord<=20){
#     x_val = 1
#   }else if(x_coord<=40){
#     x_val=2
#   }
# }

# #helper timing function from stackoverflow
# #time things with s.t({ x = 5+3 })
# #https://stackoverflow.com/questions/17515193/r-how-to-make-system-time-print-the-expression-being-timed
# s.t <- function(expr, msg="", verbose=TRUE, gcFirst=FALSE, title="") { 
#   # wrapper for system.time
#   # title is an alternate for msg, where user needs simply give a name to the section being timed.
#   # msg is for a custome message before the sytem.time output
#   
#   ret <- capture.output(system.time(expr=expr, gcFirst=gcFirst))
#   ret <- paste(ret, collapse="\n")
#   
#   if (nchar(title))
#     msg <- paste0("Time to complete ", title, ":")
#   
#   if (verbose){
#     if (nchar(msg) == 0)
#       cat(ret)
#     else 
#       cat(paste(msg, collapse=""), ret, sep="\n")
#   }
# }

preference_type <- function(preference){
  if(preference=="none"){ #no preference
    memory_this_bee = memory_cloud[which(memory_cloud[,3]>0),]    #take all of the points that are flowers
    
  }else if(preference=="population_proportion"){ 
    cutoff=floor(blueberry_preference*n_bees)
    if(bee<cutoff){   #the first n% are blueberry bees and the rest are wildflower bees
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==2),]   #blueberry bee
    }else{
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==1),]   #wildflower bee
    }
    #probably should do something about what if the bee didn't find anything of the right type
    
  }else if(preference=="nest_proportion"){  #determine what the nest needs 
    
    total = sum(Nest_Resource)
    blueberry_proportion = Nest_Resource[2]/total
    wildflower_proportion = Nest_Resource[1]/total
    
    if(blueberry_proportion<blueberry_preference){ #the nest needs blueberry pollen
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==2),]  #take just the locations in blueberries
    }else{
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==1),]  #take just the locations in wildflowers
    }
    #probably should do something about what if the bee didn't find anything of the right type
    
  }else{ #preference="probability"
    #probabilistically select whether the memory point should be in blueberry or in wildflower
    prob = runif(1,0,1)
    if(prob<blueberry_preference){
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==2),]  #take just the locations in blueberries      
    }else{
      memory_this_bee = memory_cloud[which(memory_cloud[,3]==1),]  #take just the locations in wildflowers
    }
  }
  return(memory_this_bee)
}









memory_function_random = function(){
  if(length(memory_this_bee)>4){ #there are multiple memory points
  index = sample(1:dim(memory_this_bee)[1],1)
  return(memory_this_bee[index,])
  }else{ #there is only one memory point so you have to use it
    return(memory_this_bee)                                           #select that point
  }
}

memory_function_distance = function(){
  if(length(memory_this_bee)>4){ #there are multiple memory points
  distances = sqrt((nx-memory_this_bee[,1])^2+(ny-memory_this_bee[,2])^2)/1414   #calculate the distance of each point from the nest (normalized so max is 1)
  random_number = rbeta(n=1,shape1=2,shape2=3)                                 #generate a random number to compare them to
  new_distances = abs(distances-random_number)                              #pick the one that's closest to that number
  indices = which(new_distances==min(new_distances))                          #find its index
  if(length(indices)>1){
    index=sample(x=indices,size=1)
  }else{
    index=indices
  }
  return(memory_this_bee[index,])                                           #select that point
  }else{ #there is only one memory point so you have to use it
    return(memory_this_bee)                                           #select that point
  }
  
}

memory_function_type = function(){
if(length(memory_this_bee)>4){ #there are multiple memory points
  blb_amounts = numeric(dim(memory_this_bee)[1])                  #calculate the amount of wf around each point
  wf_amounts = numeric(dim(memory_this_bee)[1])  
  for(i in 1:dim(memory_this_bee)[1]){
    mx = memory_this_bee[i,1]
    x_values = seq(from=mx-n,to=mx+n,by=alpha)
    my = memory_this_bee[i,2]
    y_values = seq(from=my-n,to=my+n,by=alpha)
    flowervalues = matrix(NA,nrow=length(y_values),ncol=length(x_values))
    for(j in 1:length(x_values)){
      for(k in 1:length(y_values)){
        flowervalues[k,j] = embedded_landscape(x_values[j],y_values[k])
      }
    }
    wf_amounts[i] = length(which(flowervalues==1))/(length(x_values)*length(y_values))
  }
  wf_amounts - wf_amounts/max(wf_amounts)   #normalize just in case/for consistency with energy
  # random_number = rbeta(n=1,shape1=beta_params[1],shape2=beta_params[2])                                 #generate a random number to compare them to
  in_bins = findInterval(wf_amounts,vec=seq(0,1,.1),rightmost.closed=TRUE)
  prob_factor = numeric(10)
  for(i in 1:10){
    if(length(which(in_bins==i))>0){
      prob_factor[i] = 1
    }else{
      prob_factor[i] = 0
    }
  }
  random_interval = sample(1:10,size=1,prob=prob_distr*prob_factor)
  bin = which(in_bins==random_interval)
  if(length(bin)>0){
    index=sample(x=bin,size=1)
  }else{
    print("error, no points in this bin")
  }
  return(memory_this_bee[index,])                                           #select that point
  
}else{  #there is only one memory point, so you have to use it
  return(memory_this_bee)                                           #select that point
  
} #end of else                                        
  
}

memory_function_energy = function(){
  if(length(memory_this_bee)>4){ #there are multiple memory points
    blb_percents = numeric(dim(memory_this_bee)[1])  
  wf_percents = numeric(dim(memory_this_bee)[1])  
  blb_amounts = numeric(dim(memory_this_bee)[1])  
  wf_amounts = numeric(dim(memory_this_bee)[1])  
  n_blb_depleted = numeric(dim(memory_this_bee)[1])  
  n_wf_depleted = numeric(dim(memory_this_bee)[1])  
  Dists = numeric(dim(memory_this_bee)[1])  
  
  #first see how much blb and wf is around each point
  for(i in 1:dim(memory_this_bee)[1]){  #for each point in the memory cloud
    mx = memory_this_bee[i,1]
    my = memory_this_bee[i,2]
    x_values = seq(from=mx-n,to=mx+n,by=alpha)
    y_values = seq(from=my-n,to=my+n,by=alpha)
    flowervalues = matrix(NA,nrow=length(y_values),ncol=length(x_values))     #make the grid around it
    for(j in 1:length(x_values)){
      for(k in 1:length(y_values)){
        flowervalues[k,j] = embedded_landscape(x_values[j],y_values[k])       #find the flower values at the grid
      }
    }
    blb_percents[i] = length(which(flowervalues==2))/(length(x_values)*length(y_values))  #record the proportion of the points there that are blb
    wf_percents[i] = length(which(flowervalues==1))/(length(x_values)*length(y_values))  #record the proportion of the points there that are wf
    Dists[i] = 2*sqrt((mx-nx)^2+(my-ny)^2)      #record the distance to the point
  
    # #then see how many depleted blb and wf flowers there are at each point
    # temp1 = which(all_X_harvested[,2]<(mx+n))    #column 2 is blueberry 
    # temp2 = which(all_X_harvested[,2]>(mx-n))
    # temp3 = which(all_Y_harvested[,2]<(my+n))
    # temp4 = which(all_Y_harvested[,2]>(my-n))
    # x_ind_blb = intersect(temp1,temp2)         #intersect is the elements in both
    # y_ind_blb = intersect(temp3,temp4)
    # ind_blb = intersect(x_ind_blb,y_ind_blb)
    # n_blb_depleted[i] = length(ind_blb)                 #store how many depleted blb flowers there are around this point
    # 
    # temp1 = which(all_X_harvested[,1]<(mx+n))    #column 1 is wf 
    # temp2 = which(all_X_harvested[,1]>(mx-n))
    # temp3 = which(all_Y_harvested[,1]<(my+n))
    # temp4 = which(all_Y_harvested[,1]>(my-n))
    # x_ind_wf = intersect(temp1,temp2)         #intersect is the elements in both
    # y_ind_wf = intersect(temp3,temp4)
    # ind_wf = intersect(x_ind_wf,y_ind_wf)
    # n_wf_depleted[i] = length(ind_wf)                 #store how many depleted wf flowers there are around this point
  }

  
  # #Now calculate the actual amount of wf and blb
  # n_blb = blb_percents*full_square_blb - n_blb_depleted   #total-depleted = number undepleted
  # n_wf = wf_percents*full_square_wf - n_wf_depleted
  # 
  # blb_amounts = n_blb/(full_square_blb)    #undepleted/total possible = percent
  # blb_amounts[is.na(blb_amounts)]=0
  # wf_amounts = n_wf/(full_square_wf)
  # wf_amounts[is.na(wf_amounts)]=0
  
  # #now calculate the mean handling time and nectar/flower at each point
  # H_bar = (H_b*blb_amounts + H_w*wf_amounts)/2
  # R_bar = (R_b*blb_amounts + R_w*wf_amounts)/2
  # Lambda_bar = (Lambda_b*blb_amounts + Lambda_wf*wf_amounts)/2
  
  #now calculate the energy at each point (Cresswell 2000)
  #first the energy if the point was all blb/wf
  energies_blb = ( C*Lambda_b - (Dists/S)*M_fly - ((H_b*C)/R_b)*M_provision )/( (Dists/S) + ((H_b*C)/R_b) )
  energies_wf = ( C*Lambda_w - (Dists/S)*M_fly - ((H_w*C)/R_w)*M_provision )/( (Dists/S) + ((H_w*C)/R_w) )
  
  #then the average energy based on percents 
  energies = (energies_blb*blb_percents + energies_wf*wf_percents)/2
  energies[is.na(energies)]=0     #turn NAs to 0
  energies[which(energies<0)]=0   #turn negative ones to 0  
  normalized_energies = energies/max(energies)   #normalize to match the probability distribution
  #now see which one we'll pick
  # random_number = rbeta(n=1,shape1=beta_params[1],shape2=beta_params[2])   #this makes 
  in_bins = findInterval(normalized_energies,vec=seq(0,1,.1),rightmost.closed=TRUE)
  prob_factor = numeric(10)
  for(i in 1:10){
    if(length(which(in_bins==i))>0){
      prob_factor[i] = 1
    }else{
      prob_factor[i] = 0
    }
  }
  # print(prob_factor)
  random_interval = sample(1:10,size=1,prob=prob_distr*prob_factor)
  bin = which(in_bins==random_interval)
  if(length(bin)>0){
    index=sample(x=bin,size=1)
  }else{
    print("error, no points in this bin")
  }
  return(memory_this_bee[index,])     #select that point
  }else{ #there is only one memory point, so you have to use it
    return(memory_this_bee)                                           #select that point
    
  } #end of else                
}


# #This is the function for choice of memory after exploration based on energetic cost/gain
# #See Exploration_Raster_2d for generation of points
# #This is Riley's re-written code 
# 
# ######################### Memory Function #####################
# 
# #memory_function is pre-compiled from this script
# memory_function_energy = function(){
#   
#   # print("Hello!")
#   
#   mass = full/.9                             #mass of the bee
#   cost = 0.436                               #energetic cost of flying (Heinrich???)
#   
#   num_samplePos = 100 #number of random samples per position
#   
#   ##build single matrix to store bee memory positions and their respective sample positions
#   beeMem_count = dim(memory_this_bee)[1]
#   
#   idx = c(1:beeMem_count) #each memory is given an index
#   
#   posX = memory_this_bee[,1] #memories position
#   posY = memory_this_bee[,2]
#   
#   sampleX = rep(0, beeMem_count) #each memory has n sample positions
#   sampleY = rep(0, beeMem_count)
#   
#   wildCount = rep(0, beeMem_count) #number of flowers at each memories position
#   blueCount = rep(0, beeMem_count)
#   
#   beeMem = cbind(idx, posX, posY, sampleX, sampleY, wildCount, blueCount)
#   
#   ##Expand rows of beeMem matrix to allow num_samples(ie 100) samplePositions per position 
#   ##matrix now has 100 duplicates of each position
#   rowsExpanded = rep(1:beeMem_count, rep(num_samplePos, beeMem_count))
#   beeMem <- beeMem[rowsExpanded,]
#   
#   ##Create 100 sample positions for each bee memory, then add sample positions to inital matrix
#   randomOffsets <- runifdisc(n=num_samplePos*beeMem_count, centre=c(0, 0))
#   
#   beeMem[, "sampleX"] = randomOffsets$x + beeMem[, "posX"]
#   beeMem[, "sampleY"] = randomOffsets$y + beeMem[, "posY"]
#   
#   ##find depleted positions
#   
#   #depleted <- distCheck(0.1, beeMem[, "sampleX"], beeMem[, "sampleY"], DATAXharvested[!is.na(DATAXharvested)], DATAYharvested[!is.na(DATAYharvested)]) 
#   #landscape size hardcoded = bad
#   depleted = findDepleted(0.1, 2000, 2000, beeMem[, "sampleX"], beeMem[, "sampleY"], DATAXharvested[!is.na(DATAXharvested)], DATAYharvested[!is.na(DATAYharvested)]); 
#   
#   depleted_rows <- which(depleted==1,arr.ind=TRUE)#function returns a value of 1 for positions < minDist
#   
#   ##remove any depleted positions
#   
#   if(length(depleted_rows)>0){  #if any points are depleted, exclude them
#     beeMem = beeMem[-depleted_rows,]
#   }#else exclude nothing
#   
#   ##Round sample points to map coordinates
#   beeMem[, "sampleX"] = ceiling(beeMem[, "sampleX"])
#   beeMem[, "sampleY"] = ceiling(beeMem[, "sampleY"])
#   
#   values <- landscape_values[ beeMem[, c("sampleX", "sampleY")] ] #get flower types from landscape at sample positions
#   
#   ##give each sample positon either 1 wildflower or 1 blueberry depending on landscape
#   beeMem[, "blueCount"] = ifelse(values == 2, 1, 0)
#   beeMem[, "wildCount"] = ifelse(values == 2, 0, 1) #if it's not a blueberry it's a wildflower
#   
#   ##sum up the flowers at each memory position's sample positions
#   #each memory position identified by index, matrix is no longer expanded.
#   wildTotals <- rowsum(beeMem[,"wildCount"], beeMem[,"idx"])
#   blueTotals <- rowsum(beeMem[,"blueCount"], beeMem[,"idx"])
#   
#   ## Calculate the energy!
#   time <- ( sqrt((nx-posX)^2+(ny-posY)^2) )/a #time spent flying to memory locations
#   energy <- blueTotals*blueberry_energy + wildTotals*wild_energy - time*mass*cost   #the amount of energy to be gained from this memory location
#   
#   possible = which(energy==max(energy),arr.ind=FALSE) #get matrix location of positions with highest energy
#   
#   #print(paste("Riley possible:", possible))
#   
#   if(length(possible)==1){
#     index = possible
#   }else{
#     index = sample(possible, 1 )     #choose randomly from all of the highest energy points
#   }
#   
#   #print(paste("Riley index:", index))
#   
#   return(memory_this_bee[index,])
# }



embedded_landscape = function(x_coord,y_coord){
  #returns the landscape matrix value if the point is within it; otherwise, returns 0
  #values for each 1mx1m grid cell
  if(0<x_coord & x_coord<=2000 & 0<y_coord & y_coord<=2000){
    flower_value = landscape_values[ceiling(x_coord), ceiling(y_coord)]
  }else{
    flower_value = 0
  }
  return(flower_value)
}



embedded_landscape_2 = function(x_coord,y_coord){
  #returns the landscape matrix value if the point is within it; otherwise, returns 0
  #works for 100m grid cells
  if(0<x_coord & x_coord<=2000 & 0<y_coord & y_coord<=2000){
    flower_value = landscape_values[ceiling(x_coord/100), ceiling(y_coord/100)]
  }else{
    flower_value = 0
  }
  return(flower_value)
}







try_again = function(){
	# a function to try finding a memory point again - explore or randomly generate points again. 
	print("trying again") 
	
	if(point_generation=="random"){
		
		n_points = 1000

		memory_locations_ta = NULL
  
		x_points = runif(n_points,min=0,max=x_meters)     #1000 random x-coordinates; x_meters is in landscape file
  		y_points = runif(n_points,min=0,max=y_meters)     #1000 random y-coordinates y_meters is in landscape file
  		possible_mx = ceiling(x_points)    #round the points so that they are in the sub grid (1m cells) 
  		possible_my = ceiling(y_points)
  		flower_types = numeric(1000)
  
  
  		for(i in 1:1000){ #calculate the landscape values
    		flower_types[i] = embedded_landscape(possible_mx[i],possible_my[i])
  		}
  
  		bee_number = rep(bee,length(possible_mx))   #so that I can double check that all bees are getting added to memory_locations_ta
  
  		All = cbind(x_points,y_points,bee_number,flower_types)
  
  		##this needs to give x and y values, not flower field values##
	  	memory_locations_ta = All[which(All[,4]>0),]
	  	if(length(which(memory_locations_ta[,1]==nx))>0 & length(which(memory_locations_ta[,2]==ny))>0 ){
	  	nest_points =  intersect(which(memory_locations_ta[,1]==nx),which(memory_locations_ta[,2]==ny))
	  	memory_locations_ta = memory_locations_ta[-nest_points,]     #remove any points that are actually the nest	  		
	  	}
		
	}else if(point_generation=="exploration"){
		#The bee explores the landscape in a widening radius around the nest
		#flower values at the endpoint of each flight segment are recorded
		#movement is based on the Exploration_Raster version

		### Parameters

		bouts_exploration = Param_values[sample_index,12]                          #number of bouts_exploration+1 to make

		####################################################
		DATAX=nx
		DATAY=ny
#		memory_locations_ta = c(0,0,0,0)

  
 		 #################################Do the thing####################################################
		  ### Initial values and storage
  
		  bouts_exploration_counter=1
		  i = 2
		  X = rep(NA,100*bouts_exploration)                 #fill X and Y with the initial point (nest coordinates)
		  Y = rep(NA,100*bouts_exploration)
		  X[1] = nx 
		  Y[1] = ny      
		  Counter = 0
		  walks = 0                 #track how many exploring steps the bee has made
  
		  max1 = Param_values[sample_index,19]             #initial radii of looping  (D_min)
		  radius = Param_values[sample_index,20]
		  max2 = max1+radius    #D_max
		  too_long = Param_values[sample_index,22]        #e_m    #define how many steps to take   (this is different for exploration and foraging)

		  D_i = Param_values[sample_index,21]   #increase in D_min and D_max
		  e_i = Param_values[sample_index,23]   #increase in too_long
  
		  BeeID = c(3, nx, ny, runif(1,0,2*pi),0,1,0)
		  #1 population status: 1=Harvester; 0=Scout; 2 = returning to nest; 3 = exploring
		  #2,3 location: x & y coordinates of bee (nest is at (nx,ny))
		  #4 angle: direction of travel (initial angle from memory direction)
		  #5 amount of resource
		  #6 flower type to search for; 1 for wildflower and 2 for blueberry
		  #7 whether or not the bee has encountered flowers since leaving the nest (0=no,1=yes)
  
		  ### Make the bee move around! (in scouting mode only)
  
 		 while(bouts_exploration_counter<bouts_exploration){   ## do things for bouts_exploration steps ##
 	   	 theta = BeeID[4]                #the angle the bee is going at
    
    
	     if(BeeID[1] == 2){  ## what to do if the bee is done with an exploring bout
	      new_y = ny
	      new_x = nx                           #send bee back to nest
	      BeeID[1] = 3                                #make the bee an explorer
	      BeeID[4] = runif(1,0,2*pi)                 #new random angle 
	      bouts_exploration_counter = bouts_exploration_counter+1            #the bee has completed another bout
	      max1 = max1+D_i             #increase initial radii of looping
	      max2 = max1+radius
	      too_long = too_long+e_i
      
	     }else if(BeeID[1]==3){## The bee is still exploring
	      theta = BeeID[4]                             #the angle the bee travels at
	      advect_time = rexp(1,mu)                     #the time to travel for
	      new_x = BeeID[2]+a*cos(theta)*advect_time    #the new location the bee travels to (velocity*time)
 	      new_y = BeeID[3]+a*sin(theta)*advect_time    #the new location the bee travels to (velocity*time)		
      
 	     ## Then decide what the next move will be
	     if(walks >= too_long){  #the bee has been exploring long enough, so send it back to the nest 
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
	   i = i+1
   		}
  
  
	  ## Now calculate any potential memory locations 
  
  X = X[!is.na(X)]
  Y = Y[!is.na(Y)]
	  X_values = ceiling(X)
	  Y_values = ceiling(Y)
	  flower_values = numeric(length(X_values))
	  for(index in 1:length(X_values)){ 
	    flower_values[index] = embedded_landscape(X_values[index],Y_values[index])
	  }
	  bee_number = rep(bee,length(X_values))   #so that I can double check that all bees are getting added to memory_locations_ta
  
	  All = cbind(X,Y,bee_number,flower_values)
  
  
	  ##this needs to give x and y values where there are flowers
  		##this needs to give x and y values, not flower field values##
	  	memory_locations_ta = All[which(All[,4]>0),]
	  	if(length(memory_locations_ta)>0){
	  		if(length(which(memory_locations_ta[,1]==nx))>0 & length(which(memory_locations_ta[,2]==ny))>0 ){
	  		nest_points =  intersect(which(memory_locations_ta[,1]==nx),which(memory_locations_ta[,2]==ny))
	  		memory_locations_ta = memory_locations_ta[-nest_points,]     #remove any points that are actually the nest	  		
	  	}
	  	}
 
}
  	return(memory_locations_ta)
}



































