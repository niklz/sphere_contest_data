source("R/00_librariers.R")
source("R/01_utils.R")
source("R/02_meta_params.R")


con <- DBI::dbConnect(odbc::odbc(), "xsw")


# A&E data ---------------------------------------------------------------

ae_dat <- local({
  ae_tbl <- tbl(
    con,
    in_catalog(
      catalog = "Analyst_SQL_Area",
      schema = "dbo",
      table = "tbl_BNSSG_ECDS"
    )
  )

  aedat <- ae_tbl %>%
    filter(
      Site %in% c('RA701', 'RVJ01', 'RA7C2'),
      !is.na(Decision_To_Admit_Date)
    ) %>%
    select(
      ed_los_minutes = Departure_Time_Since_Arrival,
      departure_dt = Departure_Date,
      Departure_Time
    ) %>%
    collect()

  aedat1 <- aedat %>%
    drop_na() %>%
    mutate(
      departure_dt = paste0(departure_dt, " ", substr(Departure_Time, 1, 8))
    ) %>%
    mutate(departure_dt = as.POSIXct(departure_dt)) %>%
    select(-Departure_Time)

  aedat2 <- aedat1 %>%
    mutate(date = floor_date(departure_dt, "day")) %>%
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    filter(date >= training_begin) %>%
    filter(date <= training_end) %>%
    mutate(ed_harm = case_when(
     ed_los_minutes >= ed_threshold ~ (ed_los_minutes-ed_threshold)/ed_threshold,
     .default = 0
    )) %>%
    summarise(ed_harm = sum(ed_harm), .by = date) %>%
    arrange(date)

  aedat2 %>%
    ggplot(aes(x = date, y = ed_harm)) +
    geom_path() +
    scale_colour_manual(values = c("black", "red")) +
    scale_x_date(date_breaks = "3 month", labels = date_format("%b\n%Y")) +
    labs(
      title = "Harm related to delayes in ED"
    ) +
    theme_bw() +
    theme(
      plot.subtitle = element_text(face = "italic"),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )

  aedat2
})


# SWAST data -------------------------------------------------------------


swast_dat <- local({
swast_tbl <- tbl(con,
               in_catalog(catalog = "ABI",
                          schema = "dbo",
                          table = "vw_UrgentCare_Ambulance_SWASFT")
)

swastdat <- swast_tbl %>%
  filter(CCG == "15C", Outcome %in% c('See & Treat','See & Convey'), Incident_Colour == "Category 2") %>%
  select(`Response_Target_1_Clock Start`, `Response_Target_1_Clock Stop`) %>%
  collect()


swastdat1<-swastdat %>%
  rename('start'='Response_Target_1_Clock Start','departure_dt'='Response_Target_1_Clock Stop') %>%
  drop_na() %>%
  mutate(start=as.POSIXct(start,format="%Y-%m-%d %H:%M:%OS")) %>%
  mutate(departure_dt=as.POSIXct(departure_dt,format="%Y-%m-%d %H:%M:%OS")) %>%
  mutate(ambulance_response_time_minutes=as.numeric((departure_dt-start)/60)) %>%
  filter(ambulance_response_time_minutes>=0) %>%
  select(departure_dt,ambulance_response_time_minutes)

swastdat2<-swastdat1 %>%
  mutate(date=floor_date(departure_dt,"day")) %>%
  mutate(date=as.Date(substr(date,1,10))) %>%
  filter(date>=training_begin) %>%
  filter(date<=training_end) %>%
  mutate(ambulance_harm=case_when(
    ambulance_response_time_minutes >= ambulance_threshold ~ (ambulance_response_time_minutes - ambulance_threshold)/ambulance_threshold,
      .default = 0)
  ) %>%
  summarise(ambulance_harm = sum(ambulance_harm), .by = date) %>%
  arrange(date)

swastdat2 %>%
  ggplot(aes(x = date, y = ambulance_harm)) +
  geom_path()+
  scale_colour_manual(values=c("black","red")) +
  scale_x_date(date_breaks="3 month",date_minor_breaks="1 month",labels=date_format("%b\n%Y")) +
  labs(title="Harm related to delayed ambulance response")
  theme_bw() +
  theme(plot.subtitle=element_text(face="italic"),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  
  swastdat2
})


left_join(ae_dat, swast_dat) %>%
  pivot_longer(c(ed_harm, ambulance_harm), names_to = "exposure") %>%
  ggplot(aes(x = date, y = value, fill = exposure)) +
  geom_col()


# Write output -----------------------------------------------------------

output_dat <- local({
dat <- bind_rows(
  ae_dat %>%
    rename("value" = "median_ed_los_minutes", "harm" = "ed_harm") %>%
    mutate(threshold = ed_threshold) %>%
    mutate(var = "Time in ED for admitted patients (minutes)"),
  swast_dat %>%
    rename(
      "value" = "median_ambulance_response_time_minutes",
      "harm" = "ambulance_harm"
    ) %>%
    mutate(threshold = ambulance_threshold) %>%
    mutate(var = "Category 2 ambulance response time (minutes)")
) %>%
  mutate(acute_harm = ed_harm + ambulance_harm, .by = date) %>%
  select(date, var, value, threshold, harm, acute_harm)

dat %>% group_by(var) %>% summarise(n(),min(date),max(date))

write.csv(dat,file="data/outcomes_dat.csv",row.names=FALSE)

plot_dat1<-dat %>%
  mutate(date2=date+1) %>%
  group_by(var) %>%
  mutate(maxval=max(value))

plot_dat2<-plot_dat1 %>%
  mutate(value=0,maxval=0,var="Severe Acute Harm index",threshold=NA,harm=severe_harm) %>%
  group_by(date) %>%
  slice(1) %>%
  ungroup()

plot_dat3<-plot_dat1 %>%
  mutate(value=0,maxval=0,var="Sustained Severe Acute Harm",threshold=NA,harm=sustain_harm) %>%
  group_by(date) %>%
  slice(1) %>%
  ungroup()

plot<-bind_rows(plot_dat1,plot_dat2, plot_dat3) %>%
  mutate(var=factor(var,levels=c("Severe Acute Harm index", "Sustained Severe Acute Harm", "Median time in ED for admitted patients (minutes)","Median Category 2 ambulance response time (minutes)"))) %>%
  ggplot() +
  geom_hline(aes(yintercept=threshold),colour="red") +
  geom_line(aes(x=date,y=value,group=var)) +
  geom_segment(aes(x=date,xend=date2,
                   y=maxval*1.1,yend=maxval*1.1,colour=factor(harm)),linewidth=5) +
  scale_colour_manual(values=c("black","red")) +
  facet_wrap(~var,scales="free_y",ncol=1) +
  scale_x_date(date_breaks="3 month",date_minor_breaks="1 month",labels=date_format("%b\n%Y")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw() +
  theme(plot.subtitle=element_text(face="italic"),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank());plot



p_top <- bind_rows(plot_dat2, plot_dat3) %>%
  mutate(var=factor(var,levels=c("Severe Acute Harm index", "Sustained Severe Acute Harm", "Median time in ED for admitted patients (minutes)","Median Category 2 ambulance response time (minutes)"))) %>%
  ggplot() +
  geom_line(aes(x=date,y=value,group=var)) +
  geom_segment(aes(x=date, xend=date2, 
                   y=1, yend=1, 
                   colour=factor(harm)), linewidth=20) + # Increased linewidth to fill height
  scale_colour_manual(values=c("black","red")) +
  # Fix the visible area to be 0 to 2, centering our y=1 segment
  coord_cartesian(ylim = c(0, 2)) +
  facet_wrap(~var, ncol = 1) +
  scale_x_date(date_breaks="3 month", date_minor_breaks="1 month", labels=date_format("%b\n%Y")) +
  # Remove expansion to ensure the bar fills the vertical space better
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(
    plot.subtitle=element_text(face="italic"),
    legend.position="none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # Optional: Remove the panel grid to make the bars pop
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  );p_top


p_bottom <- plot_dat1 %>%
  mutate(var=factor(var,levels=c("Severe Acute Harm index", "Sustained Severe Acute Harm", "Median time in ED for admitted patients (minutes)","Median Category 2 ambulance response time (minutes)"))) %>%
  ggplot() +
  geom_hline(aes(yintercept=threshold),colour="red") +
  geom_line(aes(x=date,y=value,group=var)) +
  geom_segment(aes(x=date,xend=date2,
                   y=maxval*1.1,yend=maxval*1.1,colour=factor(harm)),linewidth=5) +
  scale_colour_manual(values=c("black","red")) +
  facet_wrap(~var,scales="free_y",ncol=1) +
  scale_x_date(date_breaks="3 month",date_minor_breaks="1 month",labels=date_format("%b\n%Y")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_bw() +
  theme(plot.subtitle=element_text(face="italic"),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank());p_bottom


plot <- (p_top / p_bottom + plot_layout(axes = "collect", heights = c(2, 3)))
print(plot)

png("materials/outcomes_plot.png",height=8,width=8,units="in",res=600)
print(plot)
dev.off()
dat
})











