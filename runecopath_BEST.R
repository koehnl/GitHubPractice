Ecopath_Orig <- read.csv(file = "/Users/koehnl/Desktop/Ecopath_R/CCEcopath_Testinput.csv", header = TRUE) 
Ecopath_Orig <- read.csv(file = "/Users/laurakoehn/Dropbox/Run_Ecopath/CCEcopath_Testinput.csv", header = TRUE)
Group = Ecopath_Orig$Groups
Bio = Ecopath_Orig$Biomass; PB = Ecopath_Orig$PB
QB = Ecopath_Orig$QB; EE_orig = Ecopath_Orig$EE
Yield = Ecopath_Orig$Yield; Type = Ecopath_Orig$Type
n = length(Bio)
BA = rep(0, length = n)
diet <- read.csv(file = "/Users/koehnl/Desktop/Ecopath_R/DietMatrix_test.csv", header = TRUE)
#DC=matrix(unlist(Ecopath_Orig[1:(n+1),12:(11+(n+1))]),ncol=n2)
diet <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/DietMatrix_test.csv", header = TRUE)
ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE_orig, Yield = Yield, BA = BA, Type = Type, DC = diet)
ecopathout=runecopath(ecopathinput)       # in other file - note in output get Bout not B and Eout not E
B=ecopathout$Bout
EE=ecopathout$EEout
Mp = ecopathout$Mpmat
(write.table(B, sep='\t'))
(write.table(EE, sep='\t'))

# Run Balanced Not confident model
#Ecopath_BAL <- read.csv(file = "/Users/koehnl/Desktop/Ecopath_R/CCEcopath_TestinputBALANCE.csv", header = TRUE) 
Ecopath_BAL <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/CCEcopath_InputJULY25.csv", header = TRUE)
Group = Ecopath_BAL$Groups; Bio = Ecopath_BAL$Biomass; PB = Ecopath_BAL$PB
QB = Ecopath_BAL$QB; EE_orig = Ecopath_BAL$EE
Yield = Ecopath_BAL$Yield; Type = Ecopath_BAL$Type
n = length(Bio)
BA = rep(0, length = n)
#dietbalance <- read.csv(file = "/Users/koehnl/Desktop/Ecopath_R/DietMatrix_testBALANCE.csv", header = TRUE)
dietbalance <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/DietMatrix_JULY25.csv", header = TRUE)
ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE_orig, Yield = Yield, BA = BA, Type = Type, DC = dietbalance)
ecopathout=runecopath(ecopathinput)

# COMPLETE ORIGINAL - NO BALANCES, NO CHANGES
param_orig <- read.csv(file = "/Users/koehnl/Desktop/ORIGINAL_PARAMS_nobalance.csv", header = TRUE)
Group = param_orig$Groups
Bio = param_orig$Biomass; PB = param_orig$PB
QB = param_orig$QB; EE_orig = param_orig$EE
Yield = param_orig$Yield; Type = param_orig$Type
n = length(Bio)
BA = rep(0, length = n)
diet_orig <- read.csv(file = "/Users/koehnl/Desktop/ORIGINAL_DIET_nobalance.csv", header = TRUE)
ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE_orig, Yield = Yield, BA = BA, Type = Type, DC = diet_orig)
ecopathout = runecopath(ecopathinput)

########################################################################################################################
# New June 18th 2014

# Import data and run ecopath balancing
#Ecopath_BAL <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/CCEcopath_July17_Catch.csv", header = TRUE)
#Ecopath_BAL <- read.csv(file = "/Users/laurakoehn/Dropbox/All constrained/Params8.csv", header = TRUE)
Ecopath_BAL <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/Pedigree/CalCurInputValues_CVs_Oct1.csv", header = TRUE)
Group = Ecopath_BAL$Group; Bio = Ecopath_BAL$density; PB = Ecopath_BAL$PB
QB = Ecopath_BAL$QB; EE = Ecopath_BAL$EE
Yield = Ecopath_BAL$Yield; Type = Ecopath_BAL$Type
n = length(Bio)
BA = rep(0, length = n)
dietbalance <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/Pedigree/diet_original.csv", header = TRUE)
#dietbalance <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/Pedigree/diet_original_NOpedNOnames.csv", header = TRUE)
#dietbalance <- read.csv(file = "/Users/laurakoehn/Dropbox/All constrained/Diet8.csv", header = TRUE)
ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE, Yield = Yield, BA = BA, Type = Type, DC = dietbalance)
ecopathout=runecopath(ecopathinput)
B=ecopathout$Bout
EE=ecopathout$EEout
Mp = ecopathout$Mpmat
(write.table(B, sep='\t'))
(write.table(EE, sep='\t'))


# Prepare data for trade off model
GCE = ecopathout$GCE; GCE[1,1] = 0; GCE[93,1] = 0
diet = as.matrix(dietbalance)
Import = as.vector(diet[94,])
Group.data = data.frame(Group, Biomass = B, PB, QB, EE, GCE, GroupType = Type, BA, Import)


########################
runecopath=function (ecopathinput) 
{
	n=length(ecopathinput$Biomass)
# assign input to variables
B=as.matrix(ecopathinput$Biomass)
PB=as.matrix(ecopathinput$PB)
QB=as.matrix(ecopathinput$QB)
EE=as.matrix(ecopathinput$EE)
BA=as.matrix(ecopathinput$BA)
DC = as.matrix(ecopathinput$DC)
# DC=matrix(unlist(ecopathinput[1:n,12:(11+n)]),ncol=n)
Y=as.matrix(ecopathinput$Yield)
Grouptype=as.matrix(ecopathinput$Type)

livinggroups=which(!Grouptype=="nonLiving")
nliving=length(livinggroups)
consumers=which(Grouptype=="Consumer")
detrital_groups=which(Grouptype=="nonLiving")

# Parindex is a vector of 1 and 0, 0 indicates that biomass is specified, 1
# indicates that EE is specified
Parindex=rep (0,nliving)

# find groups where  biomass is an input
Bindex=which(B>0)

# find groups where EE is an input
EEindex=which(EE>0)
n=length(B)

# setup some matrices for calculations

  Parindex[EEindex]=rep(1,length(EEindex))                       

# Calculate known consumption rates on each prey group
   	KnownCons=rep(0,nliving)
if (length(Bindex)>=0) # do this if any biomass levels are specified
   	{
     nr=length(Bindex)
   for (i in 1:nr) #cycle through all groups for which biomass is an input
      {
      KnownCons=KnownCons+B[Bindex[i]]*QB[Bindex[i]]*DC[livinggroups,Bindex[i]]
      Parindex[Bindex[i]]=2                    #2 means know biomass and calcualted consumption
   }
}

# Calculate total losses and gains for each group
G=Y[livinggroups] + KnownCons +BA[livinggroups]

# Solve for linear coefficients
a=matrix(0,nrow=nliving, ncol=nliving)
for (i in 1:nliving) {           # number of functional groups also number of equations   (rows)
   for (j in 1:nliving) {         #columns - number of unknowns 
     if (i==j) {
         if (Parindex[i]==1) {                  #if don't know biomass , yes know EE
            a[i,j]=EE[i]*PB[i]-QB[i]*DC[i,i]     #estimate B
        } else {
            a[i,j]=B[i]*PB[i]                 ##else estimate EE (Parindex=0, don't know EE, yes know B)
        }                                        
       }else if (Parindex[j]==1) {                 #if on off diagonal calc biomass entering group
            a[i,j]=-DC[i,j]*QB[j]                  #estimate consumption
        }    
   }
}

# make corrections for primary producer group
Groupindex=which(Grouptype=="Producer")
for (i in 1:length(Groupindex)) {
    index=Groupindex[i]               #if primary producer group
    if (Parindex[index]==1) {         #if primary producer group
       a[index,index]=EE[index]*PB[index]   #estimate B (leave out diet component since not eating)
    } else  {
        a[index,index]=B[index]*PB[index]          #estimate EE
}
}

# Solve equation
X=solve(a)%*%G       #matrix multiplication (inverse of matrix 'a' multiplied by vector 'G')

#Make Output
Bout=matrix(0,n,1)
EEout=matrix(0,n,1)
for (i in 1:nliving) {
   if (Parindex[i]==1) {
      Bout[i]=X[i]                   
      EEout[i]=EE[i]             
      
    } else   {
      Bout[i]=B[i]
      EEout[i]=X[i]    
     }                
}
Bout[detrital_groups] = B[detrital_groups]


if (length(detrital_groups)>0){
  Detritus_input=sum(Bout[livinggroups,1]*PB[livinggroups,1]*(matrix(1,nliving,1)-EEout[livinggroups]))
	Consumption=Bout[consumers,1]*QB[consumers,1]
	Detritus_output=DC[detrital_groups,consumers]%*%Consumption
	EE_detritus=Detritus_output/Detritus_input
	EEout[detrital_groups,1]=EE_detritus
	}

# Calculate trophic levels
#source("makeTL.R")
#TL=makeTL(DC,Grouptype)




#--------------------------------------------------------------
# Calculate Consumption and Mortalities
n_old = nrow(DC)
DC2 = DC[-n_old,]
CC=matrix(0,n,1)
for (j in 1:n) {
   # Calculate consumption (CC) on this group
   CC[j,1]=sum(matrix(DC2[j,],n,1)*QB*Bout)
}
# Calculate Predation Mortalities:
Mp=CC/Bout
# Calcluate Fishing Mortalities
FF=Y/Bout
# Calcluate Other Mortality
Mo=PB-Mp-FF-BA/Bout

# Calculate Predation Matrix

Mpmat=matrix(0,n,n)
for (i in 1:n) {
   Consumption=QB[i]*Bout[i]
   Mpmat[,i]=Consumption*DC2[,i]/Bout
}

# Calculate Gross Conversion Efficiency
GCE=PB/QB
GCE[Groupindex]=NaN
ecopathout=list(Bout=Bout, PB=PB, Consumption=Consumption, EEout=EEout, QB=QB, DC=DC, BA=BA, Mpmat=Mpmat, Mo=Mo, GCE=GCE, FF=FF, Grouptype=Grouptype)
return (ecopathout)
}  #end function

                