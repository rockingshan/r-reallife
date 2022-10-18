library(tidyverse)

channel_master = read.csv(file.choose(new = F))  ###open channel master file
package_file = read.csv(file.choose(new = F)) #open Package Details file
combo_file = merge(package_file,channel_master,by.x = 'Channel',by.y = 'Channel',all.x = T)
#if needed only pay channels run following block
br_rm = c('ABP News Network Pvt Limited','Free to AIR','DD','Discontinued Channel','Republic TV')
combo_file = filter(combo_file, !(Broadcaster %in% br_rm))
#block end
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
plan_list = plan_names[['Plan.Name']]
for (package in plan_list) {
  combo_file_flt = filter(combo_file, Package == package) %>% select(Package,Channel,Genre) %>% unique()
  combo_file_flt$Genre = toupper(combo_file_flt$Genre)
  combo_file_flt$Channel = toupper(combo_file_flt$Channel)
  genre_file = combo_file_flt %>% group_by(Genre) %>% summarise(Channel_list = paste(Channel,collapse=","))
  write.csv(genre_file,sprintf("Output/%s_.csv",package),row.names = F)
  }