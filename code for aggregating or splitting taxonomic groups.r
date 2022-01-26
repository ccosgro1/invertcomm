###########################
### Import OTU Taxonomy table
# The rows in this table must be a perfect match to the columns in the sitesXspecies matrix
# you will need to change the path for your computer, or import the data the way you like to
#otu.tax=read.csv("invert_taxonomy_file.csv")
otu.tax=read.csv("C:/Users/crc31/Dropbox/Jennings Invertebrates/flyingFFG.csv")
p=nrow(otu.tax)
dim(otu.tax)
names(otu.tax)

#Remove drosophilidae from otu.tax
#otu.tax=otu.tax[otu.tax$family!="Drosophilidae",]


#Remove all non-dip taxa
otu.tax=otu.tax[otu.tax$FFG!="",]
commdata=

###########################
### AGGREGATE OTUs to higher level taxonomic groups 
# before pasting in analyses from other file below.

# Choose a level using the next line
# finest=1, order=2, family=3, genus=4
# Then look at the groups in the chosen taxonomic level.
# This example creates a dataset of abundances for each class, and throws out sequences not validly assigned to a class (because including them would be meaningless)
# Note that columns 1-10 in this dataset are the metadata, and there is some hard-coding column references around that 
level=4
levels(as.factor(otu.tax[,level]))
types=levels(as.factor(otu.tax[,level]))
q=length(types)
n=length(commdata[,1])
data.agg=matrix(nrow=n,ncol=q,0)
colnames(data.agg)=types
for(j in 1:p){
	i=match(otu.tax[j,level],types)
	data.agg[,i]=data.agg[,i]+commdata[,j+7]
}
# A column, labelled "FALSE", is generated that shows the number of sequences that could not be adequately
# classified to the requested level.  The next line removes this column.
data.agg=data.agg[,colnames(data.agg)!="FALSE"]
data.agg=cbind(commdata[,1:7],data.agg)
dim(data.agg)
write.csv(data.agg,"invertcomm_genus.csv")

###############




###########
### SPLIT off a chosen group to perform an analysis of OTUs just within the group
# before pasting in analyses from other file below.

# Define a taxonomic level of interest. 
# Phylum=3, Class=4, Order=5, Family=6, Genus=7
# Then look at the groups in the chosen taxonomic level.

# Let's select phylum Nematoda
# So this example creates a dataset of Nematoda-only OTUs
level=5
i=7
levels(as.factor(otu.tax[,level]))
# Choose your group in the next line. Make sure spelling is perfect.  Don't choose "FALSE" because that is meaningless.
spp.only=commdata[,8:158]
group="woodfeeding"
data.nematoda=spp.only[,otu.tax[,level]==group]
dim(data.nematoda)

#data.nematoda=data.nematoda[c(1:18,22:38),]
data.agg=data.nematoda
data.agg=cbind(commdata[,1:7],data.agg)
data.agg


date4.agg=data.agg[data.agg$Date=="10.15.16",]
date2.agg=data.agg[data.agg$Date=="5.5.16",]
date3.agg=data.agg[data.agg$Date=="8.10.16",]
date1.agg=data.agg[data.agg$Date=="10.6.15",]
colsum1=colSums(date1[8:97])
colsum2=colSums(date2[8:97])
colsum3=colSums(date3[8:97])
colsum4=colSums(date4[8:97])

  
#remove drosophilidae
colnames(commdata)
commdata=commdata[c(1:80,84:158),]
dim(commdata)  
