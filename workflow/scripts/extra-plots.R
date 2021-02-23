# map for all caves
register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
map <- get_stamenmap(c(right = -57, left = -96, top = 52, bottom = 32), 
                     zoom = 5, source = "stamen", maptype = "terrain-background", scale = "auto")
require(maps)
ggplot()+
  borders("world") +
  geom_sf(aes(geometry=presence.df$geoms,fill=presence.df$WNS_MAP_YR))+
  coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
  # transition_reveal(along = presence.df$WNS_MAP_YR, keep_last = T)+ 
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/county-incidence.png",dpi=300)