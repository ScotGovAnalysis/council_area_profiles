produce_CA_content <- function(area, full_dataset) {
  
  # we take our full dataset input and throw it 
  # into this function's local environment
  # this allows us to keep the code exactly as it was 
  # when it ran as a script at global level
  list2env(full_dataset, envir = environment())
  # we don't the input object anymore
  # and we don't want to return it 
  rm(full_dataset)
  # Define paramsarea ======================================================================
  # we also put our council area name 
  # and put it where the original area was before this 
  # 
  paramsarea <- area
  #paramsarea <- "West Lothian"
  
  # ==========================
  # from here on this is the original script 
  # that ran at the beginning of the markdown
  # ==========================
  
  source("content scripts/1-data.R",local = T)
  # print(111111111111111)
  source("content scripts/2-plots.R",local = T)
  # print(222222222222222)
  source("content scripts/3-tables.R",local = T)
  # print(333333333333333)
  source("content scripts/4-text.R",local = T)
  # print(444444444444444)
  # ==========================
  # This is where the original script ends
  # we now repack the function's environment 
  # into a list to return upwards
  # ==========================
  
  # here we gather all our environment vars set above
  # into a single list for passing back out of the function
  CA_data = mget(ls(environment()))
  
  # If we wanted to add anything to the list we pass back
  # we should do so here
  
  # CA_data$new_bit = table blah blah blah
  
  return(CA_data)
  
}

