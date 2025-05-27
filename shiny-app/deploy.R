#load library
library(rsconnect)

#set up path
rsconnect::setAccountInfo(name = 'mattsada',
                          token = '5E4DA24AF146CA871C2F9E7657C54EEB',
                          secret = 'CMKrmQedHY5/ww0kRRYBQF8Npzr/vcclSTeQaphz')

#deploy
rsconnect::deployApp('shiny-app/', appName = "AssignSampleIDs")

#locally deploy
shiny::runApp("shiny-app")
