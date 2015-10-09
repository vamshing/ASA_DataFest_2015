# 1.LOAD DATA 

set.seed(2038947, kind = NULL, normal.kind = NULL)
options(scipen=999)
setwd("C:/Users/Vamshi/Desktop/Datafest/DF2015 Data")
visitor<-read.csv("visitor.csv",header=T,stringsAsFactors=F, fill=T)
tran<-read.csv("transactions.csv",header=T,stringsAsFactors=F)
leads<-read.csv("leads.csv",header=T,stringsAsFactors=F, fill=T)

# 2.EXTERNAL DATA, RUKA CODES ON ZIP CODES 

ruca <- read.csv("C:/Users/Abhishek/Desktop/Datafest/rucafile1.csv",colClasses=c(rep("factor",5)))
config<-read.csv("configuration.csv",header=T,stringsAsFactors=F, fill=T)
shop<-read.csv("shopping.csv",header=T,stringsAsFactors=F, fill=T)
#Sub-setting the visitor dataset
test<-data.frame(visitor$visitor_key,visitor$session_count,visitor$first_session_start_datetime,visitor$last_session_start_datetime,visitor$new_page_views,visitor$used_page_views,visitor$cpo_page_views,visitor$preprod_page_views,visitor$new_dwell_time,visitor$used_dwell_time,visitor$cpo_dwell_time,visitor$preprod_dwell_time,visitor$tot_dwell_time,visitor$page_views,visitor$leasing_art,visitor$sell_adv_art,visitor$saft_adv_art,visitor$fuel_eco_art,visitor$drv_tip,visitor$mnth_loan_pay_calc,visitor$auto_lease_calc,visitor$afford_calc,visitor$inc_reb_calc,visitor$gas_guzz_calc,visitor$inc_car,visitor$inc_index_car,visitor$dlr_rev_index_inv,visitor$ddp_sale_rev_inv,visitor$buy_guide_carrev,visitor$buy_guide_know_carrev,visitor$buy_guide_index_carrev,visitor$feat_art_carrev,visitor$comparator,visitor$maint_calls_maint,visitor$maint_howto_maint,visitor$new_finder,visitor$features_nmydp,visitor$safety_nmydp,visitor$mpg_nmydp,visitor$lt_rd_test_nmydp,visitor$comp_testrev_nmydp,visitor$used_cars_tmv_appraiser,visitor$pc_adv,visitor$pc_calc,visitor$pc_car_inc,visitor$pc_car_rev,visitor$pc_car_inv,visitor$pc_comparator,visitor$pc_maint,visitor$pc_new_finder,visitor$pc_new_mydp,visitor$pc_appr,visitor$pc_used_mydp,visitor$drr_sales_submit_count,visitor$drr_service_submit_count,visitor$make_count,visitor$model_count,visitor$submodel_count,visitor$modelyear_count,visitor$style_count,visitor$zip)

#3.RANDOM SAMPLING OF THE DATA

visitor_sample<-test[sample(1:nrow(test), 10000), ]
#Creating a visitor short-list
visitor_zip<-data.frame(visitor_sample$visitor.visitor_key,visitor_sample$visitor.zip)
colnames(visitor_zip)[1:2] <- c("visitor_key","zip")
visitor_zip[visitor_zip==""] <- NA
visitor_zip_noblank<-na.omit(visitor_zip)
leads_short<-data.frame(leads$visitor_key,leads$make,leads$lead_id,leads$make,leads$dealer_distance)
colnames(leads_short)[1:3] <- c("visitor_key","make","lead_id")
leads_short$make <- toupper(leads_short$make)

#4.ROLLING UP THE TABLE ON NUMBER OF LEADS TO VISITOR LEVEL

leads_rollup<-aggregate(lead_id ~ visitor_key + make, leads_short, function(x) length(unique(x)))
leads_rollup[leads_rollup==""] <- NA
leads_rollup<-na.omit(leads_rollup)

#5.MERGE SHOPPING DATA WITH LEADS DATA 

visitor_shop<-merge(visitor_zip_noblank,shop,by.x="visitor_key",by.y="visitor_key")
visitor_shop$make_name <- toupper(visitor_shop$make_name)
shop_rollup<-aggregate(model_name ~ visitor_key + zip + make_name, visitor_shop, function(x) length(x))

#6.AGGREGRATE VISITOR DATA WITH ZIPCODES & RUKA CODES - ROLL UP BY VISITOR_KEY

visitor_ruca <- merge(visitor_zip_noblank,ruca,by.x="zip",by.y="ZIPA" , all.x=TRUE)
visitor_ruca_new <- visitor_ruca[,c(-3,-4,-5)] 
visitor_ruca_noruca<-na.omit(visitor_ruca_new)
leads_overall<-aggregate(lead_id ~ visitor_key,leads_rollup,FUN=sum)
shop_overall<-aggregate(model_name ~ visitor_key,shop_rollup,FUN=sum)
leads_entropy_base<-merge(leads_rollup,leads_overall,by="visitor_key")
colnames(leads_entropy_base)[3:4]=c("leads_indiv","leads_overall")
shop_entropy_base<-merge(shop_rollup,shop_overall, by="visitor_key")
colnames(shop_entropy_base)[3:5]=("make","shop_indiv","shop_overall")

#7. SHANNON'S ENTROPY TABLES - LEADS AND SHOPPING

#Base tables
leads_entropy_base$entropy<-leads_entropy_base$leads_indiv/leads_entropy_base$leads_overall
shop_entropy_base$entropy<-shop_entropy_base$shop_indiv/shop_entropy_base$shop_overall

#Calculations
leads_entropy_base$shannon_entropy<-((-1)*leads_entropy_base$entropy * log(leads_entropy_base$entropy,10))
shop_entropy_base$shannon_entropy<-((-1)*shop_entropy_base$entropy * log(shop_entropy_base$entropy,10))

#ROLL UP BY VISITOR_KEY
leads_entropy_base1<-aggregate(leads_entropy_base$shannon_entropy ~ visitor_key,leads_entropy_base,FUN=sum)
shop_entropy_base1<-aggregate((shop_entropy_base$shannon_entropy) ~ visitor_key+zip,shop_entropy_base,FUN=sum)

#8.MERGE ALL

table_00 <- merge(shop_entropy_base1,leads_entropy_base1,by.x="visitor_key" ,by.y="visitor_key", all.x=TRUE)

#MERGE the final table with RUCA Codes
table_01 <- merge(table_00,ruca,by.x="zip",by.y="ZIPA",all.x=TRUE)
table_02 <- table_01[,c(-5,-6,-7)]
table_02[table_02==""] <- NA
table_02<-na.omit(table_02)
colnames(table_02)[3:4]<-c("shop_entropy","leads_entropy")
table_02$rucanew<-as.numeric(table_02$RUCA)
table_02$urbanicity <- ifelse(table_02$rucanew >5,"Rural","Urban")
table_02_ru <- table_02[which(table_02$urbanicity=='Rural'), ]
table_02_urb <- table_02[which(table_02$urbanicity=='Urban'), ]

#9.T-TEST FOR RURAL VS URBAN CUSTOMERS

t.test(table_02_urb[,3],table_02_ru[,3])
t.test(table_02_urb[,4],table_02_ru[,4])

#10.PLOTS

hist(table_02_urb[,3],xlab="Diversity of Brands - Shopping",ylab="Frequency",main="Urban Population")
hist(table_02_urb[,4],xlab="Diversity of Leads submitted",ylab="Frequency",main="Urban Population")
hist(table_02_ru[,3],xlab="Diversity of Brands - Shopping",ylab="Frequency",main="Rural Population")
hist(table_02_ru[,4],xlab="Diversity of Leads submitted",ylab="Frequency",main="Rural Population")
#INSIGHT : an urban shopper behaves completely different to a rural shopper - BOTH LEADS AND SEARCHES

#Normal Plots
library(ggplot2)

# PLOT OVERALL DENSITY - SHOPS
ggplot(data = table_02,aes(x=table_02[,3],bin=.10))  + geom_density(color = "blue",size=0.8) + labs(x = "Entropy",y ="Density",title = "Overall Population Density for Shopping Diversity") 

# PLOT OVERALL DENSITY - LEADS
ggplot(data = table_02,aes(x=table_02[,4],bin=.10))  + geom_density(color = "violet",size=0.8) + labs(x = "Entropy",y ="Density",title = "Overall Population Density for Leads Diversity") 

# PLOT rural DENSITY - SHOPS
ggplot(data = table_02_ru,aes(x=table_02_ru[,3],bin=.10))  + geom_density(fill="blue") + labs(x = "Entropy",y ="Density",title = "Rural Population Density for Shopping Diversity") +coord_cartesian(,ylim = c(0,12.5),xlim = c(0,1.1))

# PLOT rural DENSITY - LEADS
ggplot(data = table_02_ru,aes(x=table_02_ru[,4],bin=.10))  + geom_density(fill="magenta") + labs(x = "Entropy",y ="Density",title = "Rural Population Density for Leads Diversity")+coord_cartesian(,ylim = c(0,35),xlim = c(0,0.6))

# PLOT Urban DENSITY - SHOPS
ggplot(data = table_02_urb,aes(x=table_02_urb[,3],bin=.10))  + geom_density(fill="blue") + labs(x = "Entropy",y ="Density",title = "Urban Population Density for Shopping Diversity")+coord_cartesian(,ylim = c(0,19.5),xlim = c(0,1.1))

# PLOT Urban DENSITY - LEADS
ggplot(data = table_02_urb,aes(x=table_02_urb[,4],bin=.10))  + geom_density(fill = "violet") + labs(x = "Entropy",y ="Density",title = "Urban Population Density for Leads Diversity")+coord_cartesian(,ylim = c(0,40),xlim = c(0,0.6))
temp_data <- data.frame(dens = c(table_02[,3], table_02[,4]), lines = rep(c("Shopping Diversity", "Leads Diversity")))
ggplot(temp_data, aes(x = dens, fill = lines)) + geom_density() + coord_cartesian(,ylim = c(0,25),xlim = c(0,1.1))
 
 
