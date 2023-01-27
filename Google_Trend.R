library(gtrendsR)

recession <- gtrendsR::gtrends("Recession", onlyInterest = TRUE, geo = "CA", time = "all")
recession1 <-data.frame(recession$interest_over_time)


inflation <- gtrendsR::gtrends("Inflation", onlyInterest = TRUE, geo = "CA", time="all")
inflation1 <-data.frame(inflation$interest_over_time)


supplyChain <- gtrendsR::gtrends("Supply chain", onlyInterest = TRUE, geo = "CA", time="all")
supplyChain1 <-data.frame(supplyChain$interest_over_time)

write.csv(recession1, file   = paste0("Data/Recession_trends.csv")  , row.names = F)
write.csv(inflation1, file   = paste0("Data/Inflation_trends.csv")  , row.names = F)
write.csv(supplyChain1, file = paste0("Data/SupplyChain_trends.csv"), row.names = F)
