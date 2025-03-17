#Random generation for possible memory points

# print("generating random points")

memory_locations = NULL

for(bee in 1:n_bees){
  
x_points = runif(1000,min=0,max=x_meters)     #1000 random x-coordinates; x_meters is in landscape file
y_points = runif(1000,min=0,max=y_meters)     #1000 random y-coordinates y_meters is in landscape file
# points = as.data.frame(runifdisc(n=1000,radius=900,centre=c(1000,1000)))   #1000 possible(x,y) pairs in a circle centered at (1000,1000) with radius 900
possible_mx = ceiling(x_points)    #round the points so that they are in the sub grid (1m cells) 
possible_my = ceiling(y_points)
flower_types = numeric(1000)


for(i in 1:1000){ #calculate the landscape values
  flower_types[i] = embedded_landscape(possible_mx[i],possible_my[i])
}

bee_number = rep(bee,length(possible_mx))   #so that I can double check that all bees are getting added to memory_locations
# bout_number = rep(bouts_counter,length(X_values))  #same for all bouts

All = cbind(possible_mx,possible_my,bee_number,flower_types)

##this needs to give x and y values, not flower field values##
memory_locations =  rbind(memory_locations,All[which(All[,4]>0),])

}
memory_cloud = unique(memory_locations[-1,])

#if any points are the nest, remove them
nest_points =  intersect(which(memory_cloud[,1]==nx),which(memory_cloud[,2]==ny))
if(length(nest_points)>0 ){
  memory_cloud = memory_cloud[-nest_points,]     #remove any points that are actually the nest	  		
}

Bees_Found_Flowers = unique(memory_cloud[,3])

n_bees_found_flowers = length(Bees_Found_Flowers)
BeesFoundFlowers[sim_number] = n_bees_found_flowers
