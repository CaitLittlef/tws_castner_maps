

## Extract values for all GAP12 and Castner Range
start <- Sys.time()
# gap12_vals <- exact_extract(lc, conus_gap12)
gap12_vals <- exact_extract(lc, w_gap12)
print(Sys.time() - start) #10 min

gap12_vals <- bind_rows(gap12_vals) %>%
  group_by(value) %>%
  summarise(pixel_cnt = round(sum(coverage_fraction),0)) %>%
  left_join(lu.lc, by = c("value" = "Value")) %>%
  filter(value > 100) # < 100 is human-dominated

# If still way too large, spplit into gap 1 and 2
# gap1 <- conus_gap12[conus_gap12$GAP_Sts == "1",] 
# gap2 <- conus_gap12[conus_gap12$GAP_Sts == "2",] 

remove(conus_gap12)

start <- Sys.time()
cr_vals <- exact_extract(lc, cr)
print(Sys.time() - start)

cr_vals <- bind_rows(cr_vals) %>%
  group_by(value) %>%
  summarise(pixel_cnt = round(sum(coverage_fraction),0)) %>%
  left_join(lu.lc, by = c("value" = "Value")) %>%
  filter(value > 100) # < 100 is human-dominated



## Summarize by macrogroups, format, etc.
## CHECK TO SEE IF THIS IS W US OR ALL CONUS!!

mgrp <- gap12_vals %>%
  group_by(macrogroup) %>% 
  filter(macrogroup != "") %>% # some still missing (eg glacier, ice)
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  arrange(perc_area)

frmt <- gap12_vals %>%
  group_by(ivc_format) %>%
  filter(ivc_format != "") %>%
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  arrange(perc_area)

mgrp_cr <- cr_vals %>%
  group_by(macrogroup) %>%
  filter(macrogroup != "") %>%
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  arrange(perc_area)

frmt_cr <- cr_vals %>%
  group_by(ivc_format) %>%
  filter(ivc_format != "") %>%
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  arrange(perc_area)

(w <- as.character(c(frmt$ivc_format)))
water <- w[c(2,3,4,5,7,10)]

ggplot(data = frmt[frmt$perc_area>=1 & !frmt$ivc_format %in% water,],
       aes(x = reorder(ivc_format, -perc_area), y = perc_area)) +
  geom_col() +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggplot(data = frmt_cr[frmt_cr$perc_area>=1,],
       aes(x = reorder(ivc_format, -perc_area), y = perc_area)) +
  geom_col() + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))



## Look within warm desert

gap12_wd <- gap12_vals %>% filter(ivc_format == "Warm Desert & Semi-Desert Scrub & Grassland")
cr_wd <- cr_vals %>% filter(ivc_format == "Warm Desert & Semi-Desert Scrub & Grassland")

# Total area in Warm Desert & Semi-Desert Scrub & Grassland
a <- sum(gap12_wd$pixel_cnt) * res(lc)[1]^2 # 87604578000

sys_wd <- gap12_wd %>%
  group_by(system_nam) %>%
  filter(system_nam != "") %>%
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  arrange(perc_area)

sys_cr_wd <- cr_wd %>%
  group_by(system_nam) %>%
  filter(system_nam != "") %>%
  summarise(pixel_cnt = round(sum(pixel_cnt))) %>%
  mutate(perc_area = 100*pixel_cnt/sum(pixel_cnt)) %>%
  # mutate(perc_area = 100*pixel_cnt*(res(lc)[1]^2)/a) %>%
  arrange(perc_area)


ggplot(data = sys_wd[sys_wd$perc_area>=1,],
       aes(x = reorder(system_nam, -perc_area), y = perc_area)) +
  geom_col() +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggplot(data = sys_cr_wd[sys_cr_wd$perc_area>=1,],
       aes(x = reorder(system_nam, -perc_area), y = perc_area)) +
  geom_col() + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

## Combine into one df
# sys_wd$range <- "all"
# sys_cr_wd$range <- "cr"
# boo <- sys_wd %>%
#   left_join(sys_cr_wd, by = "system_nam")
# 
# ?left_join()

# # Not using; would require more spreading. But shows ahve to persist order
sys_wd$range <- "all"
sys_cr_wd$range <- "cr"
# boo <- rbind(sys_wd[sys_wd$perc_area >=1,], sys_cr_wd)
boo <- rbind(sys_cr_wd[sys_cr_wd$perc_area >=1,], sys_wd[sys_wd$perc_area >=1,])


# 
boo %>%
  group_by(range) %>% # defines this as a grouped df
  arrange(desc(perc_area), .by_group = TRUE) %>% # sorts and remembers the grp
  mutate(name=factor(system_nam, levels=system_nam)) %>%  # updates factor levels
  ggplot(aes(x = name, y = perc_area)) +
  geom_col() +
  scale_x_discrete(labels = function(x)
    stringr::str_wrap(x, width = 15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  facet_wrap(~range, ncol = 1)


