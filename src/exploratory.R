# exploratory data analysis

library(janitor)

rm(list=ls())

# load data
load("C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/hornsoe.RData")
load("C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/sec_dat.RData")

names(sec_dat) <- janitor::make_clean_names(names(sec_dat))

dat <- left_join(hornsoe, sec_dat, by = c("OBJECTID"="objectid") )

dat$dist_street <- as.numeric(dat$dist_street)
dat$dist_stream <- as.numeric(dat$dist_stream)
dat$dist_prwlne <- as.numeric(dat$dist_prwlne)
dat$dist_pond <- as.numeric(dat$dist_pond)
dat$dist_lake <- as.numeric(dat$dist_lake)


variables_of_interest <- c("HANSYN",     # Conservation cosideration area in ha
                           "SKOGSMHA",   # Productive forestarea, ha
                           "NETTOHA",    # Netto ha
                           "LAF",        # Lowest age for forest cutting
                           "VOLYM",      # Volume per ha
                           "DIAMETER",   # Average diameter
                           "HOJD",       # Average height
                           "ALDER",      # Average stand age
                           "MARKSLAG",   # Marktype: FM = solid ground, FMT = Solid ground with peat, TM = Peat ground
                           "JORDDJUP",   # Soildepth
                           "JORDART",    # Soiltype
                           "KORNSTORL",  # Grain size
                           "MARKFUKT",   # Soil moisture
                           "MARKVATTEN", # Soil water (S = missing, K = short periods, L = Long periods)
                           "BARIGHET",   # Soil Barrenness: the higher the grade, the worse its capacity
                           "YTSTR",      # Soil surfface structure, higher number shows more stones and boulders
                           "LUTN",       # Inclination
                           "LUTNRIKTN",  # Inclination direction
                           "DIKAD",      # If ditches/trenches have been created
                           "DEGEN"       # Degenerated
                           
                           
)

voi_secondary <- names(sec_dat)[2:length(names(sec_dat))]

voi <- append(variables_of_interest, voi_secondary)

variable_labels <- list(HANSYN ~ "Cons. consideration [ha]", 
                        SKOGSMHA ~ "Prod. forest [ha]",
                        NETTOHA ~ "Netto area [ha]",
                        LAF ~ "Lowest age for forest cutting",
                        VOLYM ~ "Volume per ha",
                        DIAMETER ~ "Average diameter",
                        HOJD ~ "Average height",
                        ALDER ~ "Average stand age",
                        MARKSLAG ~ "Marktype: FM = solid ground, FMT = Solid ground with peat, TM = Peat ground",
                        JORDDJUP ~ "Soildepth",
                        JORDART ~ "Soiltype",
                        KORNSTORL ~ "Grain size",
                        MARKFUKT ~ "Soil moisture",
                        MARKVATTEN ~ "Soil water (S = missing, K = short periods, L = Long periods)",
                        BARIGHET ~ "Soil Barrenness: higher = worse",
                        YTSTR ~ "Soil surface structure; higher = more stones",
                        LUTN ~ "Inclination",
                        LUTNRIKTN ~ "Aspect",
                        DIKAD ~ "Ditches/trenches created",
                        DEGEN ~ "Degenerated",
                        dist_street ~ "Dist. street",
                        dist_stream ~ "Dist. stream",
                        dist_prwlne ~ "Dist. powerline",
                        dist_pond ~ "Dist. pond",
                        dist_lake ~ "Dist. lake",
                        friction ~ "Friction",
                        friction_walking_only ~ "Friction walking", 
                        loss_2000 ~ "FL 2000",
                        loss_2001 ~ "FL 2001",
                        loss_2002 ~ "FL 2002",
                        loss_2003 ~ "FL 2003",
                        loss_2004 ~ "FL 2004",
                        loss_2005 ~ "FL 2005",
                        loss_2006 ~ "FL 2006",
                        loss_2007 ~ "FL 2007",
                        loss_2008 ~ "FL 2008",
                        loss_2009 ~ "FL 2009",
                        loss_2010 ~ "FL 2010",
                        loss_2011 ~ "FL 2011",
                        loss_2012 ~ "FL 2012",
                        loss_2013 ~ "FL 2013",
                        loss_2014 ~ "FL 2014",
                        loss_2015 ~ "FL 2015",
                        loss_2016 ~ "FL 2016",
                        loss_2017 ~ "FL 2017",
                        loss_2018 ~ "FL 2018",
                        loss_2019 ~ "FL 2019",
                        loss_2020 ~ "FL 2020",
                        loss_2021 ~ "FL 2021",
                        loss_2022 ~ "FL 2022",
                        bdod_0_5cm_mean ~ "BDOD", 
                        cec_0_5cm_mean ~ "CEC",
                        cfvo_0_5cm_mean ~ "Cfvo",
                        clay_0_5cm_mean ~ "Clay",
                        sand_0_5cm_mean ~ "Sand",
                        silt_0_5cm_mean ~ "Silt",
                        nitrogen_0_cm_mean ~ "Nitrogen",
                        phh2o_0_5cm_mean ~ "Ph",
                        soc_0_5cm_mean ~ "SOC",
                        elev ~ "Elevation (SRTM)",
                        slope ~ "Slope (SRTM)",
                        aspect ~ "Aspect (SRTM)")


#tbl_summary(d %>% st_drop_geometry(), 
#            include = all_of(variables_of_interest),
#            by = "NVMALKL")





# Plot the map. group = group connects the points in the correct order
ggplot(data = d[d$park=="hornsu",], aes(fill = treat)) + 
  geom_sf() 


tbl_summary(dat %>% st_drop_geometry(), 
            include = all_of(voi),
            type = list(c("HANSYN", "SKOGSMHA", "NETTOHA", "LAF", "VOLYM", 
                          "DIAMETER", "HOJD", "ALDER", 
                          "BARIGHET", "YTSTR", "LUTN", 
                          "DIKAD", "DEGEN", voi_secondary) ~ "continuous", 
                        c("JORDDJUP","JORDART","KORNSTORL","MARKFUKT", 
                          "LUTNRIKTN", "MARKSLAG", "MARKVATTEN") ~ "categorical"),
            label = variable_labels,
            statistic = list(all_continuous() ~ "{mean} ({sd})", 
                             all_categorical() ~ "{n} ({p}%)"),
            by = "treatment") %>%
  add_p()




### exploratory regression
dat$treated <- ifelse(dat$treatment == "Conservation", 1, 0)
f1 <- as.formula(paste("treated", paste(voi, collapse = "+"), sep = "~"))
m1 <- lm(f1, data = dat)

summary(m1)

