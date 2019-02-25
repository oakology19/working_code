# Load your packages!!!

library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(reshape)
library(plyr)
install.packages("reshape")

# Load in all the data. This is all you should need. Don't touch!!

SCA_oak <- read.csv("G:/working/jaz/Climate_Oak/SCA.csv")
SRI_oak <- read.csv("G:/working/jaz/Climate_Oak/SRI.csv")
SCR_oak <- read.csv("G:/working/jaz/Climate_Oak/SCR.csv")
SCR_oak <- SCR_oak[-c(13,14,15,21,22,23,28,29,30,31,32,75,77,78),]
COMBO_oak <- read.csv("G:/working/jaz/Climate_Oak/COMBO.csv")
all_climate <- read.csv("G:/working/jaz/Climate_Oak/combo_climates.csv")
SCA_climate <- read.csv("G:/working/jaz/Climate_Oak/SCA_climate.csv")
SCR_climate <- read.csv("G:/working/jaz/Climate_Oak/SCR_climate.csv")
SRI_climate <- read.csv("G:/working/jaz/Climate_Oak/SRI_climate.csv")

mpi <- read.csv("G:/working/jaz/Climate_Oak/projected_clim/final/mpi.csv")
miroc <- read.csv("G:/working/jaz/Climate_Oak/projected_clim/final/miroc.csv")
proj <- read.csv("G:/working/jaz/Climate_Oak/projected_clim/final/combo_climates_proj.csv")

SCA_proj <- filter(proj, island == "SCA" | island == "SCA_mpi" | island == "SCA_miroc")
SCR_proj <- filter(proj, island == "SCR" | island == "SCR_mpi" | island == "SCR_miroc")
SRI_proj <- filter(proj, island == "SRI" | island == "SRI_mpi" | island == "SRI_miroc") 


# Set the color palette for the polygons!! Choose whichever you like!!
cols <- c("#347C98", "#C21460", "#FCCB1A", "#25596D", "#8C0F45", "#C79C03", "#58A7C6", "#EB418B", "#FDE58B")

colsSCA <- c("#347C98", "#25596D","#85BFD5")
colsSCR <- c("#C21460","#8C0F45", "#FBB2D1")
colsSRI <- c("#fad038", "#FCCB1A", "#FDEBAA", "#C79C03")


colsproj <- c("#347C98", "#25596D","#85BFD5", # Blues
              "#C21460","#8C0F45", "#FBB2D1", # Purples
              "#FCCB1A", "#C79C03", "#FDEBAA") # Yellows


########################################## Graphs: DENSITY, POINTS, HULLS!

# TMX_Present------------------------

#TMX vs PPT
find_hull <- function(all_climate) all_climate[chull(all_climate$tmx, all_climate$ppt), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = tmx, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= ppt, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_tmx.png")


# TMX vs TMN
find_hull <- function(all_climate) all_climate[chull(all_climate$tmx, all_climate$tmn), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = tmx, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmn, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmn, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_tmx.png")

# TMX vs CWD
find_hull <- function(all_climate) all_climate[chull(all_climate$tmx, all_climate$cwd), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = tmx, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= cwd, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= cwd, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_tmx.png")

# TMX_proj --------------

#TMX vs PPT
find_hull <- function(proj) proj[chull(proj$tmx, proj$ppt), ] 
hulls <- ddply(proj, "island", find_hull) 


ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = tmx, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmx, fill = island, color = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= ppt, y = tmx, color = island), alpha = .2)


ggsave("G:/working/jaz/Climate_Oak/ppt_tmx_proj.png")


########TMX vs PPT for SRI ######
find_hull <- function(SRI_proj) SRI_proj[chull(SRI_proj$tmx, SRI_proj$ppt), ] 
hulls <- ddply(SRI_proj, "island", find_hull) 

fillSRI <- c("#e0b106", "#FCCB1A", "#fbe183", "#C79C03")
colsSRI <- c("#e0b106", "#FCCB1A", "#C79C03", "#fbe183")

ggplot() +
  geom_point(data = SRI_oak, aes(x= ppt, y = tmx, color = "blue")) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = fillSRI) +
  scale_color_manual("", values = colsSRI) +
  theme_minimal() +
  geom_density2d(data = SRI_proj, aes(x= ppt, y = tmx, color = island), alpha = .5) +
  ggtitle("Santa Rosa Island") +
  xlab("Precipitation (mm)") + 
  ylab("Maximum Summer Temperatue (°C)") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18)) +
  theme(plot.title = element_text(size = 20, face = "bold"))


ggsave("G:/working/jaz/Climate_Oak/ppt_tmx_proj_SRI.png")


#TMX vs PPT for SCR ###########
find_hull <- function(SCR_proj) SCR_proj[chull(SCR_proj$tmx, SCR_proj$ppt), ] 
hulls <- ddply(SCR_proj, "island", find_hull) 

fillSCR <- c("#C21460",  "#8C0F45", "#FBB2D1")
colsSCR <- c("#b9135b", "#C21460", "#8C0F45", "#FBB2D1")

ggplot() +
  geom_point(data = SCR_oak, aes(x= ppt, y = tmx, color = "blue")) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = fillSCR) +
  scale_color_manual("", values = colsSCR) +
  theme_minimal() +
  geom_density2d(data = SCR_proj, aes(x= ppt, y = tmx, color = island), alpha = .5) +
  ggtitle("Santa Cruz Island") +
  xlab("Precipitation (mm)") + 
  ylab("Maximum Summer Temperatue (°C)") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18))+
  theme(plot.title = element_text(size = 20, face = "bold"))


ggsave("G:/working/jaz/Climate_Oak/ppt_tmx_SCR_proj.png")


# TMX vs TMN
find_hull <- function(proj) proj[chull(proj$tmx, proj$tmn), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = tmx, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmn, y = tmx, fill = island, color = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmn, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_tmx_proj.png")

# TMX vs CWD
find_hull <- function(proj) proj[chull(proj$tmx, proj$cwd), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = tmx, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= cwd, y = tmx, fill = island, color = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= cwd, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_tmx_proj.png")



#TMN_Present----------------------

#TMN VS PPT
find_hull <- function(all_climate) all_climate[chull(all_climate$tmn, all_climate$ppt), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = tmn, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= ppt, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_tmn.png")


# tmn vs TMX
find_hull <- function(all_climate) all_climate[chull(all_climate$tmn, all_climate$tmx), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = tmn, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmx, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmx, y = tmn, color = island), alpha = .5)

ggsave("G:/working/jaz/Climate_Oak/tmx_tmn.png")

# tmn vs CWD
find_hull <- function(all_climate) all_climate[chull(all_climate$tmn, all_climate$cwd), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = tmn, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= cwd, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= cwd, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_tmn.png")

#TMN_proj===========================

#TMN VS PPT
find_hull <- function(proj) proj[chull(proj$tmn, proj$ppt), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = tmn, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= ppt, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= ppt, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_tmn_proj.png")


# tmn vs TMX
find_hull <- function(proj) proj[chull(proj$tmn, proj$tmx), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = tmn, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmx, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmx, y = tmn, color = island), alpha = .5)

ggsave("G:/working/jaz/Climate_Oak/tmx_tmn_proj.png")

# tmn vs CWD
find_hull <- function(proj) proj[chull(proj$tmn, proj$cwd), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = tmn, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= cwd, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= cwd, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_tmn_proj.png")


#TMN vs CWD for SCA ###########
find_hull <- function(SCA_proj) SCA_proj[chull(SCA_proj$tmn, SCA_proj$cwd), ] 
hulls <- ddply(SCA_proj, "island", find_hull) 

fillSCA <- c("#347C98", "#25596D","#85BFD5")
colsSCA <- c("#2e6c85", "#347C98", "#25596D","#85BFD5")

ggplot() +
  geom_point(data = SCA_oak, aes(x= cwd, y = tmn, color = "blue")) +
  geom_polygon(data = hulls, aes(x= cwd, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = fillSCA) +
  scale_color_manual("", values = colsSCA) +
  theme_minimal() +
  geom_density2d(data = SCA_proj, aes(x= cwd, y = tmn, color = island), alpha = .5) +
  ggtitle("Santa Catalina Island") +
  xlab("Climate Water Deficit (mm)") + 
  ylab("Minimum Winter Temperatue (°C)") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18)) +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave("G:/working/jaz/Climate_Oak/cwd_tmn_SCA_proj.png")



#PPT_present ------------------------


#ppt vs tmx
find_hull <- function(all_climate) all_climate[chull(all_climate$tmx, all_climate$ppt), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = ppt, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmx, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmx, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_ppt.png")

# ppt vs TMN
find_hull <- function(all_climate) all_climate[chull(all_climate$ppt, all_climate$tmn), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = ppt, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmn, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmn, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_ppt.png")

# ppt vs CWD
find_hull <- function(all_climate) all_climate[chull(all_climate$ppt, all_climate$cwd), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = ppt, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= cwd, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= cwd, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_ppt.png")




#PPT_proj ------------------------


#ppt vs tmx
find_hull <- function(proj) proj[chull(proj$tmx, proj$ppt), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = ppt, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmx, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmx, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_ppt_proj.png")

# ppt vs TMN
find_hull <- function(proj) proj[chull(proj$ppt, proj$tmn), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = ppt, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmn, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmn, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_ppt_proj.png")

# ppt vs CWD
find_hull <- function(proj) proj[chull(proj$ppt, proj$cwd), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= cwd, y = ppt, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= cwd, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= cwd, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_ppt_proj.png")






#CWD_present--------------------------


#cwd vs tmx
find_hull <- function(all_climate) all_climate[chull(all_climate$cwd, all_climate$tmx), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = cwd, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmx, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmx, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_cwd.png")

# cwd vs TMN
find_hull <- function(all_climate) all_climate[chull(all_climate$cwd, all_climate$tmn), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = cwd, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= tmn, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= tmn, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_cwd.png")

# cwd vs ppt
find_hull <- function(all_climate) all_climate[chull(all_climate$ppt, all_climate$cwd), ] 
hulls <- ddply(all_climate, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = cwd, color = Island)) +
  scale_color_manual("", values = cols) +
  geom_polygon(data = hulls, aes(x= ppt, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= ppt, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_cwd.png")







#CWD_proj--------------------------


#cwd vs tmx
find_hull <- function(proj) proj[chull(proj$cwd, proj$tmx), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmx, y = cwd, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmx, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmx, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_cwd_proj.png")

# cwd vs TMN
find_hull <- function(proj) proj[chull(proj$cwd, proj$tmn), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= tmn, y = cwd, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= tmn, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= tmn, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_cwd_proj.png")

# cwd vs ppt
find_hull <- function(proj) proj[chull(proj$ppt, proj$cwd), ] 
hulls <- ddply(proj, "island", find_hull) 

ggplot() +
  geom_point(data = COMBO_oak, aes(x= ppt, y = cwd, color = Island)) +
  scale_color_manual("", values = colsproj) +
  geom_polygon(data = hulls, aes(x= ppt, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = colsproj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= ppt, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_cwd_proj.png")









#FOG_present------------------------------


fog_climate <- filter(all_climate, island == "SRI" | island == "SCR")

fog_points <- filter(COMBO_oak, Island == "SRI" | Island == "SCR")

fog_cols <- c("#C21460", "#FD4D0D")

# TMX vs FOG

find_hull <- function(fog_climate) fog_climate[chull(fog_climate$tmx, fog_climate$fog), ] 
hulls <- ddply(fog_climate, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = tmx, color = Island)) +
  scale_color_manual("", values = fog_cols) +
  geom_polygon(data = hulls, aes(x= fog, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= fog, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_fog.png")

# TMN vs FOG

find_hull <- function(fog_climate) fog_climate[chull(fog_climate$tmn, fog_climate$fog), ] 
hulls <- ddply(fog_climate, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = tmn, color = Island)) +
  scale_color_manual("", values = fog_cols) +
  geom_polygon(data = hulls, aes(x= fog, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= fog, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_fog.png")

# PPT VS FOG

find_hull <- function(fog_climate) fog_climate[chull(fog_climate$ppt, fog_climate$fog), ] 
hulls <- ddply(fog_climate, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = ppt, color = Island)) +
  scale_color_manual("", values = fog_cols) +
  geom_polygon(data = hulls, aes(x= fog, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= fog, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_fog.png")

# CWD VS FOG

find_hull <- function(fog_climate) fog_climate[chull(fog_climate$cwd, fog_climate$fog), ] 
hulls <- ddply(fog_climate, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = cwd, color = Island)) +
  scale_color_manual("", values = fog_cols) +
  geom_polygon(data = hulls, aes(x= fog, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols) +
  theme_minimal() +
  geom_density2d(data = all_climate, aes(x= fog, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_fog.png")


#FOG_proj--------------------



fog_climate_proj <- filter(proj, island == "SRI" | island == "SCR" |
                        island == "SRI_miroc" | island == "SCR_miroc" |
                        island == "SRI_mpi" | island == "SCR_mpi")

fog_points <- filter(COMBO_oak, Island == "SRI" | Island == "SCR")

fog_cols_proj <- c("#C21460","#8C0F45", "#FBB2D1", # Purples
              "#FCCB1A", "#C79C03", "#FDEBAA") # Yellows


# TMX vs FOG

find_hull <- function(fog_climate_proj) fog_climate_proj[chull(fog_climate_proj$tmx, fog_climate_proj$fog), ] 
hulls <- ddply(fog_climate_proj, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = tmx, color = Island)) +
  scale_color_manual("", values = fog_cols_proj) +
  geom_polygon(data = hulls, aes(x= fog, y = tmx, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols_proj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= fog, y = tmx, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmx_fog_proj.png")

# TMN vs FOG

find_hull <- function(fog_climate_proj) fog_climate_proj[chull(fog_climate_proj$tmn, fog_climate_proj$fog), ] 
hulls <- ddply(fog_climate_proj, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = tmn, color = Island)) +
  scale_color_manual("", values = fog_cols_proj) +
  geom_polygon(data = hulls, aes(x= fog, y = tmn, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols_proj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= fog, y = tmn, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/tmn_fog_proj.png")

# PPT VS FOG

find_hull <- function(fog_climate_proj) fog_climate_proj[chull(fog_climate_proj$ppt, fog_climate_proj$fog), ] 
hulls <- ddply(fog_climate_proj, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = ppt, color = Island)) +
  scale_color_manual("", values = fog_cols_proj) +
  geom_polygon(data = hulls, aes(x= fog, y = ppt, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols_proj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= fog, y = ppt, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/ppt_fog_proj.png")

# CWD VS FOG

find_hull <- function(fog_climate_proj) fog_climate_proj[chull(fog_climate_proj$cwd, fog_climate_proj$fog), ] 
hulls <- ddply(fog_climate_proj, "island", find_hull) 

ggplot() +
  geom_point(data = fog_points, aes(x= fog, y = cwd, color = Island)) +
  scale_color_manual("", values = fog_cols_proj) +
  geom_polygon(data = hulls, aes(x= fog, y = cwd, fill = island), alpha = .2) +
  scale_fill_manual("", values = fog_cols_proj) +
  theme_minimal() +
  geom_density2d(data = proj, aes(x= fog, y = cwd, color = island), alpha = .5) 

ggsave("G:/working/jaz/Climate_Oak/cwd_fog_proj.png")



