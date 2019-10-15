start_time<-Sys.time()

Area <- c("Aberdeen City", "Aberdeenshire","Angus","Argyll and Bute","City of Edinburgh","Clackmannanshire",
          "Dumfries and Galloway","Dundee City","East Ayrshire","East Dunbartonshire","East Lothian",
          "East Renfrewshire", "Falkirk","Fife","Glasgow City","Highland","Inverclyde","Midlothian","Moray",
          "Na h-Eileanan Siar","North Ayrshire", "North Lanarkshire", "Orkney Islands","Perth and Kinross",
          "Renfrewshire", "Scottish Borders", "Shetland Islands","South Ayrshire","South Lanarkshire","Stirling",
          "West Dunbartonshire","West Lothian")

for (i in Area) {
  print(i)
  rmarkdown::render("NRS-Website-Council_Area_Profiles-2018_update-1-Report.Rmd",
                    output_file = paste0(gsub(" ", "-", tolower(i)),"-council-profile.html"), 
                    params = list(area=i))  
}
end_time <- Sys.time()

print(run_time <- end_time - start_time)