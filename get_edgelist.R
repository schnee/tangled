library(readr)
library(dplyr)

source("./functions.R")



tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

# check the version of the data found on disk and compare it to the
# docs.google.com version if they are identical, then there's nothing to do, so
# bail out. If they are identical and maybe you feel that they should not be, it
# could be because the Google CDN hasn't updated.
#
# you can skip over this code in interactive mode
old_state_fn <- "./data/tangled.csv"
if(file.exists(old_state_fn)){
  old_state <- read_csv(old_state_fn)
  if(identical(tangled, old_state)){
    # nothing to do; bail out
    print("Old state == current state")
  } else {
    # update the old state.
    tangled %>% write_csv(old_state_fn)
  }
} else {
  # update the old state.
  tangled %>% write_csv(old_state_fn)
}