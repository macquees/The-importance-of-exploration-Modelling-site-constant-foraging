#Create cells

n_fields_x = 20
n_fields_y = 20
n_fields = n_fields_x*n_fields_y
field_width_x = 100                   #width of fields in meters (x-dimension)
field_width_y = 100                   #width of fields in meters (y-dimension)
x_meters = n_fields_x*field_width_x   #x dimension of landscape in meters
y_meters = n_fields_y*field_width_y   #y dimension of landscape in meters

landscape_skeleton <- raster(ncol = n_fields_x,nrow = n_fields_y,xmn=0,xmx=x_meters,ymn=0,ymx=y_meters)   #creates the "skeleton"
# values(x) = 1      #fill with one value only for single type landscape
cell_values = sample(c(0,1,2),ncell(landscape_skeleton),replace=TRUE,prob=c(p_e,p_w,p_b))
values(landscape_skeleton) = cell_values
#fills in the values with 0, 1, or 2, (nothing, wildflower, blueberry) randomly selected according to probabilities

LANDSCAPE_PROPORTIONS[sim_number,] = c(length(which(cell_values==2)),length(which(cell_values==1)),length(which(cell_values==0)))/n_fields
LANDSCAPE_PROPORTIONS_Resource[sim_number,] = c(length(which(cell_values==2)),length(which(cell_values==1)))/(length(which(cell_values>0)))
#save the landscape proportion of each resource type to compare to proportion collected

#Divide cells to 1mx1m
landscape_details = disaggregate(landscape_skeleton,fact=c(field_width_x,field_width_y))    #break up x into smaller cells, keeping the values

if(plot_the_landscape==TRUE){   #just plot an image of the landscape
  landscape_plotting = flip(landscape_skeleton,2)  #landscape raster to use for plotting (flipped to put x[1,1] at bottom left)
  plot(landscape_plotting, xlab = "x", ylab = "y", xlim = c(0,x_meters), ylim = c(0,y_meters),asp=TRUE,col=c("grey85","yellow1","lightcyan"))
  rm(landscape_plotting)
}

if(plotting==TRUE){ #to use in plotting the bee tracks later
  landscape_plotting = flip(landscape_skeleton,2)  #landscape raster to use for plotting (flipped to put x[1,1] at bottom left)
}

landscape_values = t(as.matrix(landscape_details,nrow=x_meters,ncol=y_meters, byrow=TRUE))    #landscape matrix to use for evaluating landscape value in movement model (flipped to put x[1,1] at bottom left)


## Create the window for analysis of counts
W = owin(xrange=c(0,x_meters),yrange=c(0,y_meters))

rm(landscape_details)
rm(landscape_skeleton)
