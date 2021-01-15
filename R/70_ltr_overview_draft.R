#number of unique listings by platform
ltr_unique %>% 
  st_drop_geometry() %>% 
  group_by(kj) %>% 
  summarize(n=n(), perc=n()/62073)


#number of unique listings by platform
ltr_unique %>% 
  st_transform(32610) %>% 
  st_join(LA,.) %>% 
  st_drop_geometry() %>% 
  group_by(area) %>% 
  summarize("Number of listings"=n(), "Listings out of total listings"=n()/62073) %>% 
  View()


# Number of listings posted
ltr %>% 
  st_drop_geometry() %>% 
  group_by(created) %>% 
  filter(!is.na(created)) %>% 
  summarize(n=n()) %>% 
  ggplot()+
  geom_line(aes(created, n), alpha = 0.3)+
  geom_smooth(aes(created, n), se=FALSE)+  
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3)+
  theme_minimal()+  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings")+
  ggtitle("Number of listings posted")


ltr %>% 
  st_drop_geometry() %>% 
  group_by(created, furnished) %>% 
  filter(!is.na(furnished)) %>% 
  summarize(n=n(), average_price=mean(price, na.rm=TRUE)) %>% 
  ggplot()+
  geom_line(aes(created, n, colour=furnished), alpha = 0.3)+
  geom_smooth(aes(created, n, colour=furnished), se=FALSE)+  
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3)+
  theme_minimal()+  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings")+
  ggtitle("Number of listings posted by furnished status")


# average price of postings
ltr %>% 
  st_drop_geometry() %>% 
  filter(price >= 500, price <=10000) %>% 
  group_by(created, furnished) %>% 
  filter(!is.na(furnished)) %>% 
  summarize(n=n(), average_price=mean(price, na.rm=TRUE)) %>% 
  ggplot()+
  geom_line(aes(created, average_price, colour=furnished))+
  geom_smooth(aes(created, average_price, colour=furnished), se=FALSE)+  
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3)+
  theme_minimal()+  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Average price of listings", 
                     labels = scales::dollar_format(prefix = "$"))


#average price of listings per month
ltr %>% 
  st_drop_geometry() %>% 
  filter(price >= 500, price <=10000) %>% 
  group_by(month=floor_date(created, "month")) %>% 
  summarize("Average price of listings"=mean(price, na.rm=TRUE)) %>% 
  View()


#number of listings per area
ltr_unique %>% 
  st_transform(32610) %>% 
  st_join(LA,.) %>% 
  group_by(area) %>% 
  count() %>% 
  ggplot()+
  geom_sf(aes(fill=n))+
  theme_void()+
  ggtitle("Number of unique listings posted on Kijiji and Craiglist per area")+
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80")  +
  guides(fill = guide_colourbar(title = "Listings per area",
                                title.vjust = 1)) 


#number of listings as a share of dwellings
ltr_unique %>% 
  st_transform(32610) %>% 
  st_join(LA,.) %>% 
  group_by(area) %>% 
  summarize(n=n(), listings_density=n()/(sum(dwellings)/n())) %>% 
  View()
  ggplot()+
  geom_sf(aes(fill=listings_density))+
  theme_void()+
  ggtitle("Number of unique listings posted on Kijiji and Craiglist per area")+
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80",
                       labels = scales::percent)  +
  guides(fill = guide_colourbar(title = "Listings/\ndwelling",
                                title.vjust = 1)) 






