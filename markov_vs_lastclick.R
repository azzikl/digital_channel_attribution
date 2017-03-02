library(dplyr)
library(reshape2)
library(ggplot2)
library(ChannelAttribution)
library(RGA)

#generate ga token
authorize()

#set property ID, start and end dates
#ids = propertyid
start.date = as.Date("2017/01/18")
end.date = as.Date("2017/01/18")

#getting real data via google analytics API
paths_data = get_mcf(
  ids,
  start.date = start.date,
  end.date = end.date,
  metrics = "mcf:totalConversionValue, mcf:totalConversions",
  dimensions = "mcf:mediumPath",
  filters = "mcf:conversionType==Transaction"
)

#calculating Markov and standart last click model
model = markov_model(
  paths_data,
  var_path = 'mediumPath',
  var_conv = 'totalConversions',
  out_more = TRUE
)

last_click = paths_data %>% 
  mutate(mediumPath = sub('.*>', '', mediumPath),
         mediumPath = sub(' ', '', mediumPath))

last_click = last_click %>% 
  group_by(mediumPath) %>%
  summarise(lc_conversions = sum(totalConversions)) 

# comparing two models
comparison <- merge(last_click, model$result, by.x = 'mediumPath', by.y = 'channel_name')
names(comparison) = c("medium","last_click","markov_model")

#ploting transactions 
for_plot = melt(comparison, id = "medium")

ggplot(for_plot, aes(medium, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")
