library(tidyverse)
library(DT)
library(scales)


data = read.csv("20240516-MercerUniversity-1_unverified.csv")

MS = data %>% filter(PlateLocSide < 0.8 & PlateLocSide > -0.8 
                     & PlateLocHeight > 1.4 & PlateLocHeight < 3.6 & PitchCall == 'BallCalled') %>% 
  mutate('Likely' = 'Strike')

MB = data %>% filter(PitchCall == "StrikeCalled" & PlateLocSide > 0.827 | 
                       PitchCall == "StrikeCalled" & PlateLocSide < -0.827 | 
                       PitchCall == "StrikeCalled" & PlateLocHeight < 1.38 | 
                       PitchCall == "StrikeCalled" & PlateLocHeight > 3.719) %>% 
  mutate('Likely' = 'Ball')


Sdata = rbind(MB,MS)

SDataA = 
Sdata %>% 
  select(PitchNo, Batter, Inning, Balls, Strikes, Outs, PitchCall, KorBB, Likely, 
         PlateLocHeight, PlateLocSide) %>% 
  rename("Outcome" = 'KorBB') %>% 
  mutate(Outcome = case_match(Outcome, 'Undefined' ~ "", 'Strikeout' ~ "K", 'Walk' ~ 'BB')) %>% 
  mutate(Count = paste(Balls,Strikes, sep = "-"), .after = Inning, .keep = "unused") %>% 
  mutate(PitchCall = case_match(PitchCall, 'StrikeCalled' ~ 'Strike', 'BallCalled' ~ 'Ball')) %>%
  mutate(
    PlateSide = round(PlateLocSide*12, 1),
    PlateHeight = round(PlateLocHeight*12, 1),
    HDFZ = ifelse(abs(PlateSide) > 8.49, abs(PlateSide) - 8.49, NA) %>% round(1),
    VDFZ = ifelse(PlateHeight > 30, PlateHeight - 42, PlateHeight - 18) %>% abs() %>% round(1),
    VDFZ = ifelse(abs(PlateSide) > 8.49, NA, VDFZ),
    HVDFZ = paste(HDFZ,VDFZ, sep = '|')
    ) %>% .[, c(1,2,3,4,5,6,7,8,11,12,13,14,15)] 
SDataA
  

# Missed Call Situations Plot ----
Sdata %>% 
  ggplot(aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
  ylim(0,5) +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  labs(color = "", title = "Overall") +
  geom_rect(aes(xmin = -0.708, xmax = 0.708333, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 2.167, xend = 0.708, yend = 2.167), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.833, xend = 0.708, yend = 2.833), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.5, xend = 0.708, yend = 2.5), 
               linetype = 2, color = 'red', linewidth = 1) +
  geom_segment(aes(x = 0.8, y = 1.4, xend = 0.8, yend = 3.6), color = 'black') +
  geom_segment(aes(x = -0.8, y = 1.4, xend = -0.8, yend = 3.6), color = 'black') +
  geom_segment(aes(x = 0.8, y = 1.4, xend = -0.8, yend = 1.4), color = 'black') +
  geom_segment(aes(x = -0.8, y = 3.6, xend = 0.8, yend = 3.6), color = 'black') +
  geom_rect(aes(xmin = -3, xmax = -1.2, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_rect(aes(xmin = 1.2, xmax = 3, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_point(size = 6) +
  geom_text(aes(x = PlateLocSide, y = PlateLocHeight, label = PitchNo), color = 'white', size = 3) +
  coord_fixed() +
  scale_color_manual(values = list("StrikeCalled" = "red3", "BallCalled" = "blue3")) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))


# Missed Call Distance Plot ----
SDataA %>% 
  ggplot(aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
  ylim(0,5) +
  xlim(-3,3) +
  labs(title = "Overall") +
  labs(x = "Horizontal Inches", y = "Verticle Inches") +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 2.167, xend = 0.708, yend = 2.167), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.833, xend = 0.708, yend = 2.833), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.5, xend = 0.708, yend = 2.5), 
               linetype = 1, color = 'red', linewidth = 1) +
  geom_segment(aes(x = 0.8, y = 1.4, xend = 0.8, yend = 3.6), color = 'black') +
  geom_segment(aes(x = -0.8, y = 1.4, xend = -0.8, yend = 3.6), color = 'black') +
  geom_segment(aes(x = 0.8, y = 1.4, xend = -0.8, yend = 1.4), color = 'black') +
  geom_segment(aes(x = -0.8, y = 3.6, xend = 0.8, yend = 3.6), color = 'black') +
  geom_rect(aes(xmin = -3, xmax = -1.2, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_rect(aes(xmin = 1.2, xmax = 3, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_point(size = 6) +
  geom_text(aes(x = PlateLocSide, y = PlateLocHeight, label = HDFZ ), color = 'white', size = 3) +
  geom_text(aes(x = PlateLocSide, y = PlateLocHeight, label = VDFZ), color = 'white', size = 3) +
  coord_fixed() +
  scale_color_manual(values = list("Strike" = "red3", "Ball" = "blue3")) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10)) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank(), axis.text = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Heatmap Testing -----
data %>% filter(PitchCall == "StrikeCalled") %>% 
ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
        scale_fill_gradientn(colours = c("white", 'yellow', "orange", "red")) +
        geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
        geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', size = 1) +
        geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', size = 1) +
        geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
        geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
        geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
        geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
        geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
        ylim(0, 5) + xlim(-3, 3) +
        theme(panel.background = element_blank()) +
        theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) +
        theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
              legend.key = element_blank()) +
        guides(fill = FALSE) 


run_values = data.frame(
Count = c('3-0','3-1','2-0','3-2','1-0','2-1','0-0','1-1','2-2','0-1','1-2','0-2'),
strike_rv = c(-0.117,-0.066,-0.062,-0.294,-0.035,-0.069,-0.037,-0.054,-0.209,-0.051,-0.171,-0.150),
ball_rv = c(0.051,0.168,0.143,0.234,0.088,0.064,0.032,0.048,0.085,0.024,0.038,0.021)
)

SdataA = 
Sdata %>% 
  select(PitchNo, BatterTeam, Pitcher, Batter, Inning, Balls, Strikes, Outs, PitchCall, KorBB, Likely) %>% 
  rename("Outcome" = 'KorBB') %>% 
  mutate(Outcome = case_match(Outcome, 'Undefined' ~ "", 'Strikeout' ~ "K", 'Walk' ~ 'BB')) %>% 
  mutate(Count = paste(Balls,Strikes, sep = "-"), .after = Inning, .keep = "unused") %>% 
  mutate(PitchCall = case_match(PitchCall, 'StrikeCalled' ~ 'Strike', 'BallCalled' ~ 'Ball'))

SdataA


SdataB = 
merge(SdataA, run_values, by = 'Count', all.x = TRUE) %>% 
  mutate(RV = ifelse(PitchCall == "Ball", ball_rv, strike_rv))

SdataC =
  select(SdataB, PitchNo, Inning, Pitcher, Batter, Count, Outs, PitchCall, Outcome, Likely, RV) %>% 
  .[order(.$PitchNo),]


SdataB

sum(SdataB$RV[SdataB$BatterTeam == "MER_BEA"])

sum(SdataB$RV[SdataB$BatterTeam == "SIE_SAI"])

data.frame(Balls, Strikes, Missed_Balls, Missed_Strikes, Accuracy, MU_ERV, OPP_ERV, FVR) %>% 
  mutate('Missed Balls' = Missed_Balls,
         'Missed Strikes' = Missed_Strikes,
         'MU ERV' = MU_ERV,
         'OPP ERV' = OPP_ERV,
         .keep = 'unused') %>% .[, c(1,2,5,6,3,7,8,4)]


FAVOR = if (MU_ERV > OPP_ERV) 'MERCER' else 'OPPONENT'




