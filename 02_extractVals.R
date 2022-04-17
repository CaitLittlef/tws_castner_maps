
############################
## EXTRACT VALUES TO AOIS ##
############################

## Define function for extracting and combining
my_extract <- function(r, p) {
  v <- exact_extract(r, p) %>% bind_rows()
  v <- v %>% group_by(value) %>% summarise(pix = round(sum(coverage_fraction),0))
  v <- v %>% left_join(lu.lc, by = c("value" = "Value")) 
  v <- v %>% filter(value > 100) # < 100 is human-dominated
  v <- v %>% filter(ivc_format != "") # nix any w/o formation to nix disturbed (eg burned)
  return(v)
}


## Extract vals for Castner Range and for all GAP 1&2 (whether CONUS or western states)
start <- Sys.time()
cr_vals <- my_extract(lc, cr)
gap12_vals <- my_extract(lc, gap12)
print(Sys.time() - start)

# Save total pixel count in GAP12 and CR
ttl_pix_gap12 <- sum(gap12_vals$pix)
ttl_pix_cr <- sum(cr_vals$pix)
# # Check ttl area in acres
# ttl_pix_cr * res(lc)[1]^2 / 4047 # approx 6606 acres; CR ~7000



## Define function for filtering and computing perc
my_summary  <- function(d, c) {
  s <- d %>% group_by(d[,c]) %>% summarise(pix = round(sum(pix),0))
  s <- s %>% mutate(perc_pix = 100*pix/sum(pix))
  # s <- s %>% mutate(perc_ttl_pix_gap12 = 100*pix/ttl_pix_gap12) # for all gap12, gives same as perc_pix
  s <- s %>% arrange(perc_pix)
  return(s)
}


frmt <- my_summary(gap12_vals, "ivc_format") 
# mgrp <- my_summary(gap12_vals, "macrogroup")
sys <- my_summary(gap12_vals, "system_nam")

frmt_cr <- my_summary(cr_vals, "ivc_format")
# mgrp_cr <- my_summary(cr_vals, "macrogroup")
sys_cr <- my_summary(cr_vals, "system_nam")


## Plot
# Apply any filters
(w <- as.character(c(frmt$ivc_format)))
water <- w[c(2,3,4,5,7,10)]

frmt <- frmt[frmt$perc_pix>=1,]
frmt <- frmt[!frmt$ivc_format %in% water,]
frmt_cr <- frmt_cr[frmt_cr$perc_pix>=1,]

# # Single variable plot
# ggplot(data = frmt,
#        aes(x = reorder(ivc_format, -perc_pix), y = perc_pix)) + # sets largest first
#   geom_col() +
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+ # wraps x-axis labels
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) # angles x-axis labels


# Combine values for facet wrap plot
frmt$range <- "All protected areas"
frmt_cr$range <- "Castner Range"

f <- rbind(frmt, frmt_cr)

plot_frmt <- f %>%
  group_by(range) %>% # defines this as a grouped df
  arrange(desc(perc_pix), .by_group = TRUE) %>% # sorts and remembers the grp
  mutate(name=factor(ivc_format, levels=ivc_format)) %>%  # updates factor levels
  ggplot(aes(x = name, y = perc_pix, fill = range)) +
  geom_col() +
  theme_minimal() + 
  ylab("Percent of area") + 
  # theme_minimal() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) + # wraps x-axis labels
  theme(strip.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, angle = 55, vjust = 0.5, hjust=0.5), # angles x-axis labels
        legend.position = "none") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_fill_manual(values=c("darkslategray4","coral1")) + 
  facet_wrap(~range, ncol = 1, scales = "free_y")

plot_frmt
ggsave(paste0(out.dir, "formation_gap12_vs_cr_", today, ".pdf"),
       plot_frmt,
       width = 8, height = 5, units = "in")
dev.off()
ggsave(paste0(out.dir, "formation_gap12_vs_cr_", today, ".png"),
       plot_frmt,
       width = 8, height = 5, units = "in")
dev.off()



## Look at systems within warm desert

gap12_wd <- gap12_vals %>%
  filter(ivc_format == "Warm Desert & Semi-Desert Scrub & Grassland")
cr_wd <- cr_vals %>%
  filter(ivc_format == "Warm Desert & Semi-Desert Scrub & Grassland")

# Total area in Warm Desert & Semi-Desert Scrub & Grassland
a <- sum(gap12_wd$pix) * res(lc)[1]^2 # 87604578000

## Summarize by system
sys <- my_summary(gap12_wd, "system_nam")
sys_cr <- my_summary(cr_wd, "system_nam")

# Apply any filters
sys <- sys[sys$perc_pix>=1,]
sys_cr <- sys_cr[sys_cr$perc_pix>=1,]

sys$range <- "All protected areas"
sys_cr$range <- "Castner Range"

s <- rbind(sys, sys_cr)

plot_sys <- s %>%
  group_by(range) %>% # defines this as a grouped df
  arrange(desc(perc_pix), .by_group = TRUE) %>% # sorts and remembers the grp
  mutate(name=factor(system_nam, levels=system_nam)) %>%  # updates factor levels
  ggplot(aes(x = name, y = perc_pix, fill = range)) +
  geom_col() +
  theme_minimal() + 
  ylab("Percent of area") + 
  # theme_minimal() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) + # wraps x-axis labels
  theme(strip.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, angle = 55, vjust = 0.5, hjust=0.5), # angles x-axis labels
        legend.position = "none") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_fill_manual(values=c("darkslategray4","coral1")) + 
  facet_wrap(~range, ncol = 1, scales = "free_y")  # free y lets axes differ ax facets

plot_sys

plot_frmt
ggsave(paste0(out.dir, "warm_desert_systems_gap12_vs_cr_", today, ".pdf"),
       plot_sys,
       width = 8, height = 5, units = "in")
dev.off()
ggsave(paste0(out.dir, "warm_desert_systems_gap12_vs_cr_", today, ".png"),
       plot_sys,
       width = 8, height = 5, units = "in")
dev.off()





sys_wd <- gap12_wd %>%
  group_by(system_nam) %>%
  filter(system_nam != "") %>%
  summarise(pix = round(sum(pix))) %>%
  mutate(perc_area = 100*pix/sum(pix)) %>%
  arrange(perc_area)

sys_cr_wd <- cr_wd %>%
  group_by(system_nam) %>%
  filter(system_nam != "") %>%
  summarise(pix = round(sum(pix))) %>%
  # mutate(perc_area = 100*pix/sum(pix)) %>%
  mutate(perc_area = 100*pix*(res(lc)[1]^2)/a) %>%
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


