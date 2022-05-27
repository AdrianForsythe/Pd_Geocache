# map for all caves

extra_plots<-function(presence.df,output){
require(maps)
require(ggmap)
require(tidyverse)
require(RColorBrewer)

register_google(key = API_KEY)

# for testing
# presence.df<-readRDS("workflow/data/presence.df.rds")
# output <-"figures/county-incidence.png"

map <- get_stamenmap(c(right = -57, left = -96, top = 52, bottom = 32),
                     zoom = 5, source = "stamen", maptype = "terrain-background", scale = "auto")

presence.df<-readRDS(presence.df)

ggplot()+
  borders("world",fill = "whitesmoke") +
  geom_sf(aes(geometry=presence.df$geoms,fill=presence.df$WNS_MAP_YR))+
  coord_sf(xlim = c(-125, -56), ylim = c(27.5, 55))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = "right",legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(output,dpi=300)
}

extra_plots(snakemake@input[[1]],snakemake@output[[1]])
