### separation of nee based on nighttime 


### read the different dataset here
## the dataset are read using All16Sites.R script

### use the reddyproc package
library(REddyProc)

## source of the code and explanation:https://rdrr.io/cran/REddyProc/man/sEddyProc_sMRFluxPartition.html
sEddyProc_sMRFluxPartition(FluxVar.s = "NEE_f", QFFluxVar.s = "NEE_fqc", 
                 QFFluxValue.n = 0, TempVar.s = "Tair_f", QFTempVar.s = "Tair_fqc", 
                 QFTempValue.n = 0, RadVar.s = "Rg", Lat_deg.n, 
                 Long_deg.n, TimeZone_h.n, T_ref.n = 273.15 + 15, 
                 Suffix.s = "", debug.l = list(useLocaltime.b = FALSE), 
                 parsE0Regression = list())


#+++ Partition NEE into GPP and respiration
EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning
EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning
EddyProc.C$sMRFluxPartition()	


vignette('useCase')
### Make the files compatible to ReddyProc
### Source: https://www.bgc-jena.mpg.de/5624918/Input-Format
### Columns needed to do it for reddyproc
### Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar
USOF1_2017$NEE_Original
USOF1_2017$LE_Original
USOF1_2017$H_Original
USOF1_2017$`Date Time`
USOF1_2017$Ta_Original
USOF1_2017$rH_Original
USOF1_2017$VPD_REddyProc

USOF1_2017$GPP_modeled
