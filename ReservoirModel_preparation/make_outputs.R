
#HW = Headwater elevation at midnight
#TW = Tailwater Elevation at midnight
#StoVol = Storage Volume in thousands of day-second-feet
#TurbQ = Turbine Flow Daily Average in cfs (cubic feet per second)
#TotalQ = Total Flow Daily Average in cfs (cubic feet per second)
#Canal = Canal Flow in cfs (cubic feet per second) - for Kentucky dam, this is the canal between the dam and Barkley dam, for Raccoon Mountain, this is the amount of water that is pumped upward to the reservoir. 

reservoir.names <- c("apalachia","blueridge","boone","chatuge","cherokee","chickamauga","douglas","fontana","fortloudoun","fortpat","guntersville","hiwassee","kentucky","meltonhill","nickajack","norris","nottely","ocoeeno1","ocoeeno3","pickwick","raccoonmt","southholston","timsford","watauga","wattsBar","wheeler","wilbur","wilson")
fold.path <- "/home/smukhop2/workfiles/Reservoir_Model/ReservoirModel_preparation/"
#file.names <- paste0(fold.path,reservoir.names,".xlsx")
#sheet.names <- c("HW","StoVol","TW","TurbQ","TotalQ")
reservoir.names.out <- c("Apalachia","BlueRidge","Boone","Chatuge","Cherokee","Chickamauga","Douglas","Fontana","FortLoudoun","FtPatrick","Guntersville","Hiwassee","Kentucky","MeltonH","Nikajack","Norris","Nottely","Ocee1","Ocee3","Pickwick","RacoonMt","SHolston","TimsFord","Watauga","WattsBar","Wheeler","Wilbur","Wilson")
reservoir.out.order <- c(24,27,22,3,10,5,7,9,8,16,14,25,4,17,12,1,2,19,18,6,21,15,11,23,26,28,20,13)
List_Parents_ids <- list("1"=c(),	"2"=c(1),	"3"=c(),	"4"=c(2,3),	"5"=c(4),	"6"=c(5),	"7"=c(),	"8"=c(6,7),	"9"=c(),	"10"=c(),	"11"=c(10),	"12"=c(8,9,11),	"13"=c(),	"14"=c(),	"15"=c(13,14),	"16"=c(15),	"17"=c(),	"18"=c(17),	"19"=c(18),	"20"=c(12,16,19),	"21"=c(),	"22"=c(20,21),	"23"=c(22),	"24"=c(),	"25"=c(23,24),	"26"=c(25),	"27"=c(26),	"28"=c(27))
f2_from_f <- c(16,17,4,13,6,20,7,9,8,5,23,15,28,11,22,10,14,19,18,27,21,3,24,1,12,25,2,26)
f_from_f2 <- c(24,27,22,3,10,5,7,9,8,16,14,25,4,17,12,1,2,19,18,6,21,15,11,23,26,28,20,13)

###start form here########################################
load(paste0(fold.path,"data/TVA.Rdata"))
load(paste0(fold.path,"data2/TVA_UncontrInQ.Rdata"))
load(paste0(fold.path,"data2/TVA_StoVol.Rdata"))

## St = St-1 + I - E - R
## Delta_S = I-E - R
## I-E : Net Inflow
## R : TotalQ


###### WRITE DATA FOR RESERVOIR MODEL ######
date.start = "2007-06-1"
date.end = "2007-06-30"
date.range = format(seq(as.Date(date.start), as.Date(date.end), "months"), "%Y-%m")
date.range.days = format(seq(as.Date(date.start), as.Date(date.end), "days"), "%Y-%m-%d")
ntime = length(date.range)
ntime
date.Num.of.days <- as.numeric()
for(i in 1:ntime){
  date.Num.of.days[i] = as.numeric(format(seq(as.Date(paste0(date.range[i],"-01")), by = "+1 month", length = 2)[2]-1,"%d"))
}


for(f in 1:28){ #28files
  f2 = f2_from_f[f]
  tmp.reservoir = reservoir.names[f]
  tmp.reservoir.out = reservoir.names.out[f]
  tmp.file.out <- paste0(fold.path,"output_files/",tmp.reservoir,".dat")
  tmp.ind.S = match(date.range,names(StoVol.monthly[[tmp.reservoir]]))
  tmp.ind.R = match(date.range,names(TotalQ.monthly[[tmp.reservoir]]))
  
  # *24*3600/43560000 cfs to acrefeet 
  # 43560 ft^3 = 1 Acre-ft

  tmp.St_1 <- StoVol.LastDays[[tmp.reservoir]][tmp.ind.S-1]*1000*24*3600/43560000 #/Thousand[acre-ft]
  tmp.St <-  StoVol.LastDays[[tmp.reservoir]][tmp.ind.S]*1000*24*3600/43560000 #/Thousand[acre-ft]
  tmp.deltaS <- tmp.St - tmp.St_1
  tmp.R <-  TotalQ.monthly[[tmp.reservoir]][tmp.ind.R]*date.Num.of.days*24*3600/43560000 # Thousand[acre-ft] in a month
  tmp.NetInflow = tmp.deltaS + tmp.R
  #tmp.UncontrInQ = UncontrInQ.monthly[[tmp.reservoir]][tmp.ind]*24*3600/43560000 #cfs
  
  tmp.NaturalFlow = tmp.NetInflow
  if (length(List_Parents_ids[[f2]]) > 0){
    for (i in 1:length(List_Parents_ids[[f2]])){
      f2.parent = List_Parents_ids[[f2]][i]
      f.parent = f_from_f2[f2.parent]
      tmp.ind.parent = match(date.range,names(TotalQ.monthly[[reservoir.names[f.parent]]]))
      tmp.R_parent = TotalQ.monthly[[reservoir.names[f.parent]]][tmp.ind.parent]*date.Num.of.days*24*3600/43560000 # Thousand[acre-ft] in a month
      tmp.NaturalFlow = tmp.NaturalFlow - tmp.R_parent
    }
  }
  tmp.out.variable = tmp.NaturalFlow #tmp.NetInflow #tmp.UncontrInQ #deltaS / tmp.NetInflow / tmp.UncontrInQ / tmp.NaturalFlow
 # print(tmp.reservoir)
  write (tmp.out.variable, file = paste0(fold.path,"output_files/",tmp.reservoir.out,"_inflow.dat"), ncolumns=length(tmp.NetInflow))
  #print(tmp.reservoir)
  #print(tmp.NaturalFlow)
}



##### Storages #####
XX <- matrix(NA, nrow=28, ncol= ntime)
file.create(paste0(fold.path,"output_files/S_initial.dat"))
file.create(paste0(fold.path,"output_files/S_obs.dat"))
for (f in 1:28){
  f2 <- reservoir.out.order[f]
  tmp.reservoir = reservoir.names[f2]
  tmp.reservoir.out = reservoir.names.out[f2]
  tmp.file.out <- paste0(fold.path,"output_files/",tmp.reservoir,".dat")
  tmp.ind = match(date.range,names(StoVol.monthly[[tmp.reservoir]]))
  tmp.St_1 <- StoVol.LastDays[[tmp.reservoir]][tmp.ind-1]*1000*24*3600/43560000 #/Thousand[acre-ft]
  tmp.St <-  StoVol.LastDays[[tmp.reservoir]][tmp.ind]*1000*24*3600/43560000 #/Thousand[acre-ft]

  write (tmp.St_1[1], file = paste0(fold.path,"output_files/S_initial.dat"), ncolumns=1, append=TRUE)
  write (tmp.St, file = paste0(fold.path,"output_files/S_obs.dat"), ncolumns=length(tmp.St), append=TRUE)
 
  print(tmp.reservoir.out)
  #print(StoVol.daily[[tmp.reservoir]])
  #rownames(tmp.St) <- NULL
  #print(tmp.St*24*3600/43560000)
  print(tmp.St)
  XX[f,] <- tmp.St
}


##### TAIL WATER #####
TWXX <- matrix(NA, nrow=28, ncol= ntime)
file.create(paste0(fold.path,"output_files/TW.dat"))
for (f in 1:28){
  f2 <- reservoir.out.order[f]
  tmp.reservoir = reservoir.names[f2]
  tmp.reservoir.out = reservoir.names.out[f2]
  tmp.ind.TW = match(date.range,names(TW.monthly[[tmp.reservoir]]))
  tmp.TW <-  TW.monthly[[tmp.reservoir]][tmp.ind.TW] 
  
  write (tmp.TW, file = paste0(fold.path,"output_files/TW.dat"), ncolumns=length(tmp.TW), append=TRUE)
  
  print(tmp.reservoir.out)
#  print(StoVol.daily[[tmp.reservoir]])
#  rownames(tmp.St) <- NULL
#  print(tmp.St*24*3600/43560000)
  print(tmp.TW)
  TWXX[f,] <- tmp.TW
}


##### USER FRACTION #####
file.create(paste0(fold.path,"output_files/usrFract.dat"))
for (f in 1:28){
  f2 <- reservoir.out.order[f]
  tmp.reservoir = reservoir.names[f2]
  tmp.reservoir.out = reservoir.names.out[f2]
  tmp.ind.TW = match(date.range,names(TW.monthly[[tmp.reservoir]]))
  tmp.Fract = rep(1/ntime, ntime)
  write(tmp.Fract, file = paste0(fold.path,"output_files/usrFract.dat"), ncolumns=length(tmp.TW), append=TRUE)
}

##### Release Values #####
file.create(paste0(fold.path,"output_files/Releases.dat"))
file.create(paste0(fold.path,"output_files/Releases_obs.dat"))
for (f in 1:28){
  f2 <- reservoir.out.order[f]
  tmp.reservoir = reservoir.names[f2]
  tmp.reservoir.out = reservoir.names.out[f2]
  tmp.file.out <- paste0(fold.path,"output_files/",tmp.reservoir,".dat")
  tmp.ind.R = match(date.range,names(TotalQ.monthly[[tmp.reservoir]]))

  tmp.R <-  TotalQ.monthly[[tmp.reservoir]][tmp.ind.R]*date.Num.of.days*24*3600/43560000 # Thousand[acre-ft] in a month
  write (tmp.R, file = paste0(fold.path,"output_files/Releases.dat"), ncolumns=1, append=TRUE)
  
  
  write (tmp.R, file = paste0(fold.path,"output_files/Releases_obs.dat"), ncolumns=length(tmp.R), append=TRUE)
 
  
#  print(f2)
 # print(tmp.reservoir.out)
#  print(tmp.R)
  
}

##### ET_coeffs.dat #####
ET.coeffs = c(0.00088, 0.00126, 0.00262, 0.00442, 0.00682, 0.0078, 0.00876, 0.00752, 0.00504, 0.00308, 0.00148, 0.00088)
file.create(paste0(fold.path,"output_files/ET_coeffs.dat"))
ET.coeffs.all = rep(ET.coeffs, ntime/12)
  write (ET.coeffs.all , file = paste0(fold.path,"output_files/ET_coeffs.dat"), ncolumns= ntime)
  

