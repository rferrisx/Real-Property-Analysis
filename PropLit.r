library(data.table)
library(lattice)
# Data from:
# https://www.census.gov/housing/hvs/index.html
# https://www.census.gov/econ/currentdata/datasets/index
# The Housing Vacancy for 2015 - 2019 ("HV.mf.csv") has to be rebuilt with only the data portion.
# This is the census table for 'Housing Vacancy" data types (See the accompanying Census readme.txt) :

cat('
dt_idx	dt_code	dt_desc	dt_unit
1	RVR	Rental Vacancy Rate	PCT
2	HVR	Homeowner Vacancy Rate	PCT
3	HOR	Homeownership Rate	PCT
4	SAHOR	Seasonally Adjusted Homeownership Rate	PCT
5	TOTAL	Total Housing Units	K
6	OCC	Occupied Housing Units	K
7	OWNOCC	Owner Occupied Housing Units	K
*8	RNTOCC	Renter Occupied Housing Units	K
9	VACANT	Vacant Housing Units	K
10	YRVAC	Year-Round Vacant Housing Units	K
11	SEASON	Seasonal Housing Units	K
*12	RENT	Vacant Housing Units for Rent	K
13	SALE	Vacant Housing Units for Sale	K
14	RNTSLD	Vacant Housing Units Rented or Sold, Not Yet Occupied	K
15	OFFMAR	Vacant Housing Units Held Off the Market	K
16	OCCUSE	Vacant Housing Units Held Off the Market and For Occasional Use	K
17	URE	Vacant Housing Units Held Off the Market and Usual Residence Elsewhere	K
18	OTH	Vacant Housing Units Held Off the Market and Vacant for Other Reasons	K
')

# Produce table q3.2019
h1 <- fread("HV.mf.data.csv")[per_idx == 255 & (dt_idx == 8 | dt_idx == 12), # Q3 2019 data types *8 and 12* only. Data in 1000s
 .(geo_idx,dt_idx,THval=as.integer(val) * 1000)][,
 .(Sum=sum(THval)),by=.(Type=dt_idx,Region=geo_idx)][,
 setnames(dcast(.SD,Region ~ Type, value.var="Sum"),
 c("Region","TotalRented","TotalVacant"))][,
 .SD[,.(VancancyRate=round(TotalVacant/(TotalRented + TotalVacant),3) * 100)], # adhoc Q3 2019 'Vacancy Rate'
 .(Region,TotalRented,TotalVacant)]

GEO <- c("US","NE","MW","SO","WE")
q3.2019 <- cbind(GEO,h1)

cat('
q3.2019
   GEO Region TotalRented TotalVacant VancancyRate
1:  US      1    43243000     3183000          6.9
2:  NE      2     8285000      477000          5.4
3:  MW      3     8472000      646000          7.1
4:  SO      4    15658000     1512000          8.8
5:  WE      5    10827000      548000          4.8
')

#GEO LEVELS			
#geo_idx	geo_code	geo_desc	
#1	US	United States	
#2	NE	Northeast	
#3	MW	Midwest	
#4	SO	South	
#5	WE	West	

# ****************************************
# lattice charts of vacancy rates		
# Data from: https://www.census.gov/housing/hvs/data/rates.html

cat('
The rental rates for 2015 - 2019 ("tab4_msa_15_19_rvr.xlsx")has to be rebuilt 
with appropriate labels into an nrow x ncol CSV with 'Year' added:

MSA		Q1	MOE1Q	Q2	MOE2Q2	Q3	MOE3Q	Q4	MOE4Q	Year
Akron,OH 	2.00	4.10	3.60	6.10	4.00	5.70			2019
Albany,NY 	7.90	6.60	10.80	7.00	15.50	9.20			2019
Albuquerq,NM	7.10	3.70	6.80	3.60	7.80	3.70			2019
Allentow,PA-NJ	7.70	7.70	3.20	5.00	0.00	0.00			2019
')

# lattice charts
RV <- fread("tab4_msa_15_19_rvr.csv")
dev.new()
RV[Year == 2019,][,barchart(MSA ~  Q3,origin = 0,col=rainbow(nrow(.SD)))]
#

dev.new()
RV[(Year == 2019) &
(grepl("CA",MSA) |
grepl("OR",MSA) |
grepl("WA",MSA)),][,barchart(MSA ~  Q3,origin = 0,col=rainbow(nrow(.SD)))]
#

dev.new()
RV[(Year != 2019) &
(grepl("CA",MSA) |
grepl("OR",MSA) |
grepl("WA",MSA)),.(MSA,Year,Q4)][,barchart(as.factor(Year) ~ Q4 | as.factor(MSA) ,allow.mutiple=TRUE,origin = 0,col=rainbow(4))]
#

dev.new()
RV[(Year == 2019) &
(grepl("CA",MSA) |
grepl("OR",MSA) |
grepl("WA",MSA)),.(MSA,Year,Q3)][,barchart(as.factor(Year) ~ Q3 | as.factor(MSA) ,allow.mutiple=TRUE,origin = 0,col=rainbow(1))]
#

# rbind 2015 - 2019 Q4 with 2019 Q3 for CA, OR, WA
yQ32019 <- RV[(Year == 2019) &
(grepl("CA",MSA) |
grepl("OR",MSA) |
grepl("WA",MSA)),.(MSA,Year,LastQuarter=Q3)]

yQ4.all.other.years <- RV[(Year != 2019) &
(grepl("CA",MSA) |
grepl("OR",MSA) |
grepl("WA",MSA)),.(MSA,Year,LastQuarter=Q4)]

dev.new()
LastQuarter <- rbind(yQ32019,yQ4.all.other.years)
LastQuarter[,barchart(as.factor(Year) ~ LastQuarter | as.factor(MSA) ,allow.mutiple=TRUE,origin = 0,col=rainbow(5))]
#	