# Import necessary packages and dataset
require(tidyverse)
dat <- read_csv("Adops & Data Scientist Sample Data - Q1 Analytics.csv")

# q1
q1 <- dat %>%
  filter(country_id == "BDV") %>% # filter by countrry_id
  group_by(site_id) %>% # gorup by site_id
  summarise(unique_users = n_distinct(user_id)) %>% # compute the number of unique user_id
  arrange(desc(unique_users)) # arrange by descending order of the number of unique user_id
q1

# q2
attr(dat$ts,"tzone") # check timezone setting of the timestamp, which is UTC in this case

q2 <- dat %>%
  filter(ts >= as.POSIXct("2019-02-03 00:00:00", tz="UTC") &
           ts <= as.POSIXct("2019-02-04 23:59:59", tz="UTC")) %>% # filter by timestamp
  group_by(user_id, site_id) %>% # group by user_id, then site_id
  filter(n() >= 10) %>% # filter to groups that have more than 10 instances
  summarize(num_vis = n()) # compute the number of visits
q2

# q3
q3 <- dat %>%
  group_by(user_id) %>% # group by user_id
  arrange(ts) %>% # arrange by timestamp
  filter(row_number()==n()) %>% # filter to include only the last rows of each group
  ungroup() %>% # ungroup to group by site_id instead of user_id
  group_by(site_id) %>% # group by site_id
  summarise(count = n()) %>% # compute the number of users whose last visit was to each site
  arrange(desc(count)) # arrange by descending order of the number of users
q3

# q4
q4 <- dat %>% 
  group_by(user_id) %>% # group by user_id
  arrange(ts) %>% # arrange by timestamp
  filter(row_number() == 1 | row_number() == n()) %>% # filter to include only the first and last rows of ecah group
  filter(length(unique(site_id))==1) %>% # filter to include only the users whose first/last visits are to the same website
  filter(n() == 2) %>% # filter out possible users who only made one visit 
  nrow()/2 # compute the number of users by dividing the number of rows by 2 (first/last visits are included in the data)
q4
