library(ape)
library(readr)

#load the tree
tree<-read.tree("LM20-248-X600-T600.tree")

#load the traits table
traits <- read_csv("traits.csv",col_types = cols(X1 = col_skip()))
colnames(traits)<-c("trait", "species")

##OPTIONAL
#In this case, the names in the table do not match with the names in the tree nodes.
#These lines fix this:
library(stringr) #load library
trait <- str_extract(tree$tip.label, "\\_(.*?)\\_") #get name between underlines
tree$tip.label  <- gsub("_", "", trait) #remove underlines

#check which species of the table contain in the tree nodes
keep_spp<-traits$species%in%tree$tip.label
#crop the species from the table that are not in the tree
traits = traits[keep_spp,]
#check whether the species in the tree and in the table are in the same order
tree$tip.label==traits$species
#if not, put in the same order
traits <- traits[match(tree$tip.label, traits$species),]

#sort de colors
cols<-setNames(c(rainbow(length(unique(traits$trait)))),sort(unique(traits$trait)))

#tree settings
boxlabel<-function(x,y,text,cex=1,bg="transparent",offset=0){
  w<-strwidth(text)*cex*1.1
  h<-strheight(text)*cex*0.4
  os<-offset*strwidth("W")*cex
  rect(x+os,y-0.5*h,x+w+os,y+0.5*h,col=bg,border=0)
  text(x,y,text,pos=4,offset=offset,font=3, cex=0.4)
}

#plot tree
par(fg="transparent")
plot(tree, cex=0.2, type="phylogram")
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
N<-Ntip(tree)
par(fg="black")
for(i in 1:Ntip(tree)) boxlabel(pp$xx[i],pp$yy[i],tree$tip.label[i],bg=cols[traits$trait[i]], cex=0.2)




