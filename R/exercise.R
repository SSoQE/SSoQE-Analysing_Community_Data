###################################################
# September 26th 2024
# Code from github.com/orgs/SSoQE
# 
# Aim:
# Simplify complex community data using and 
# exploring various standard methodology
# (community similarity ordination, classification)
# 
# We do so to test the effect of human arrival 
# on an island communities 
###################################################

###################################################
# Loading the data #
library(here)
# We start loading the pollen core of our focal island (Tenerife)
dat<-read.csv2(here("Data", "Tenerife.csv")) # to be adapted
age<-dat$age # sample ages
species<-dat[,2:ncol(dat)] # pollen "species" (taxa)
rm(dat)

head(species)
###################################################
###################################################


###################################################
# Explore the data by plotting the pollen counts 
# of any species with time.

#######################
# one solution:
plot(Poaceae ~ age,type="b",data=species,xlab="Age BP")
#######################

# OPTIONAL, but nice: 
# Use a stratigraphic plot to get a better overview over
# all species and their temporal patterns.
# Function Stratiplot in library analogue is a possible option
# Feel free to use another one. 

# install.packages("analogue")
# library(analogue)
# Stratiplot()
analogue::Stratiplot(species, age)

# only for most abundant species: 
analogue::Stratiplot(species[,colSums(species)>0.5], age)

###################################################
###################################################


###################################################
# Ordination I: Correspondence analysis (CA) #
###################################################
# Ordination methods help you to get a quick overview
# of patterns in community composition. 

# The cca() function in the vegan package allows you to run, 
# a correspondence analysis (CA) that analyses community data.
# The function will run a CA if you only provide a Community
# data matrix (in our case species).

# INFO for later: You can also use the function to run a constrained 
# correspondence analysis (CCA) that allows you to 
# explore changes in community composition with 
# environmental gradients. But there are other methods 
# like jSDMs, which are more powerful for this.   
# more information for later: browseVignettes("vegan")

# Now lets run the Correspondence analysis (CA)
# Please take time to look at the visualisation and 
# discuss what it shows (and what not)
ord<-vegan::cca(species)
plot(ord)

# The axes represent gradients in species composition.
# Species scores and site scores are the coordinates
# of the species and sites on these axes.

###################################################
summary(ord)
# Eigenvalues can be interpreted as the explained variance (importance)
# of the different axes. The first axis always explains more than 
# consecutive ones. 

# Function screeplot() allows you to visualise the eigenvalues. 
# It shows how the importance of a single axis declines and 
# may help to decide how many axes you would like to look at.

screeplot(ord)

# We only show the first two axes. 

###################################################
# If you would like to differentiate the pattern of species and 
# sites, this can be done using the argument display, where you can 
# either select species or sites (both in quotation marks).

plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(ord, display = "species", cex=0.7, col="blue")


# Now let us add time to the plots to see how species
# composition changes with time
CA1<-ord$CA$u[,1] # coordinates of first axis
CA2<-ord$CA$u[,2] # coordinates of second axis


# Plot adding the ages
plot(ord, type = "n")  # make empty plot
points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow",type="b")
text(x=CA1+0.1,y=CA2+0.1, cex=0.7,labels=age, col="blue")

###################################################
# MAIN GOAL:
# Now, we want to visualise how species composition on Tenerife 
# has changes with time. To do so, plot the strongest gradient 
# in species composition (first axis) against time.
# Humans arrived at the island about 2300 years ago. Indicate this 
# event with a vertical line
# Make the plot nice!

#######################
## one solution:
plot(CA1~age,type="b") # 
# or nicer:
plot(CA1~age,xlim=c(4500,1000),type="b",pch=21, col="red", bg="yellow",xlab="Age BP") # 
abline(v=2300,col="black") # add human arrival time at 2300 BP
#######################

###################################################
###################################################

###################################################
# INFO, not for now: 
# The CA axes are the most important gradients in species
# composition in the data. You can also test how those
# are reflecting environmental gradients.
# In vegan, this is nicely implemented in function envfit()
###################################################

# OPTIONAL, but nice: 
# There is a second island data set available. Explore how pollen
# community composition has changed on that island. 

#############
# STOP HERE #
#############



###################################################
# Similarity in species composition #
###################################################
# vegdist allows you to calculate similarity in species
# composition between communities. It transfers the community
# matrix into a distance object.
# in our case, it compares each level in the core with all the others.
# The standard setting calculates Bray–Curtis dissimilarity

dist<-vegan::vegdist(species) 
dist
# Have a look at the original community matrix as well as 
# the distance object (dist)


#####################################################
# Ordination 2: Non-metric multidimensional scaling #
#####################################################
nmds<-metaMDS(dist,  trace = TRUE) # runs the NMDS

nmds$points # extract coordinates

# NMDS 2D figure:
plot(nmds, type = "n")
points(nmds, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow",type="b")
text(nmds, labels = age, cex=0.7, col="blue")
#####################################################

#####################################################
# Cluster analysis #
#####################################################
# Function hclust is one way to run a classification, 
# allowing various different methods (see help). There are
# multiple other packages with alternatives.
tree<-hclust(dist,method = "complete") # classification 
plot(tree)
rect.hclust(tree, 5, border="red") # cut the tree forming 5 classes
# You decide on the method and the number of classes
# Please try 2-3 different methods to see the sensitivity


# FINALLY: Use the function cutree to extract the class for each 
# layer in our core. Add that information to the plot displaying the
# first ordination axis againet time by changing the point colour
# accoring to the classification result e.g. col=cutree(tree,k=2)


#######################
## one solution:
plot(CA1~age,col=cutree(tree,k=2))
abline(v=2000,col="black")
########
# nicer: 
plot(CA1~age,xlim=c(4500,1000),type="b",pch=21, bg=cutree(tree,k=2),xlab="Age BP",ylab="Orination axis 1 [system state]")
abline(v=2000,col="black")
#
# or as ggplot:
library(tidyverse)
  data_to_plot <- 
    tibble(
      system_state = CA1,
      age = age,
      group = as.factor(cutree(tree, 2))
      ) 
  data_to_plot %>% 
    ggplot2::ggplot(
      aes(
        y = system_state,
        x = age
      )
    ) +
    geom_line(
      col = "grey50",
      linewidth = 0.1
    ) + 
    geom_point(
      aes(col = group),
      size = 3
    ) +
    theme_classic() +
    scale_x_continuous(transform = "reverse") +
    labs(
      x = "Age (cal yr BP)",
      y = "system state",
      col = "Groups",
      title = "tidyverse is superior!",
      subtitle = "look how better this is"
    ) + 
    scale_color_viridis_d()  

#######################

##################################################################
# OPTIONAL: Now select and load a core from neotoma as shown by 
# Ondřej yesterday. Use the ordination to relate the major changes
#  in species composition with time. And check if a major war 
# (like the 30 years war in Europe), the plague or any other possible
# driver is visible in pollen composition of the landscape.
# Find a way to test/visualise the effect accross 1000 cores
# and write the paper.
##################################################################
