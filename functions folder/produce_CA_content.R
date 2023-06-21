produce_CA_content <- function(area, full_dataset) {
  
  # we take our full dataset input and throw it 
  # into this function's local environment
  # this allows us to keep the code exactly as it was 
  # when it ran as a script in the Rmd at global level
  list2env(full_dataset, envir = environment())
  # we don't need the input object anymore
  # and we don't want to return it at the end of the function
  rm(full_dataset)
  # Define paramsarea ======================================================================
  # we also take our council area name 
  # and put it where the original area was before this 
  # 
  paramsarea <- area
  #paramsarea <- "West Lothian"
  
  # ==========================
  # from here on this is the original script 
  # that ran at the beginning of the markdown
  # ==========================
  
  # run these scripts in the local environment
  # all objects produced end up in the local env
  # later on they are packaged up for the Rmd file
  source("content scripts/1-data.R",local = T)
  source("content scripts/2-plots.R",local = T)
  source("content scripts/3-tables.R",local = T)
  source("content scripts/4-text.R",local = T)
  
  # ==========================
  # This is where the original script ends
  # we now repack the function's environment 
  # into a list to return upwards
  # ==========================
  
  # here we gather all our environment vars set above in the scripts
  # into a single list for passing back out of the function
  # to end up in the Rmd file
  CA_data = mget(ls(environment()))
  
  # If we wanted to add anything to the list we pass back
  # we should do so here
  
  # CA_data$new_bit = table blah blah blah
  
  return(CA_data)
  
}

