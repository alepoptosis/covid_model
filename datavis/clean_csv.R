library(tidyverse)
library(RColorBrewer)
library(stringr)
theme_set(theme_minimal())

# raw dataset (one machine runs multiple iterations)
# raw = read.csv("results/2020-06-12_sizetest2.csv", header = TRUE, skip = 6)

# raw dataset (iteration distributed over multiple machines)
path = "results"
pattern = "2020-06-15_sizetest2_"

csvs = list.files(path = path, pattern = pattern, full.names = TRUE)

raw = csvs %>%
  set_names() %>%
  map_dfr( ~ read_csv(.x, col_types = cols(), skip = 6), .id = "run_num") %>% 
  select(-"[run number]") %>%
  mutate_at("run_num", ~gsub(sprintf("%s/%s|.csv", path, pattern), "", .)) %>%
  mutate_at("run_num", as.numeric) %>%
  mutate_at("run_num", ~ run_num + 1)

names(raw) = gsub("\\[|\\]|", "", names(raw))
names(raw) = gsub("\\.", " ", names(raw))
names(raw) = gsub("\\-", "_", names(raw))

######### BREEDS AND LOCKDOWN AVERAGE PLOT

# this subset contains only data on counts, run number and step
data = raw[ ,grepl("^count|^step|^run_num", names(raw))]
# this subset contains parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count|step|contacts|dead))",
                         names(raw), perl=TRUE)])

# turns data into long format for plotting
data_long = data %>% pivot_longer (
    cols = starts_with("count"),
    names_to = "breed", 
    names_prefix = "count ",
    values_to = "count"
    )

# aggregates data from all runs into an average count and stdev
data_aggr = data_long %>%
  group_by(step, breed) %>%
  summarise(mean = mean(count), stdev = sd(count))

# adds explicit upper and lower bounds to the df
data_aggr$lower = data_aggr$mean - data_aggr$stdev
data_aggr$upper = data_aggr$mean + data_aggr$stdev

# record which ticks are spent in lockdown and remove count lockdown from df
aggr_ld = data_aggr %>% 
  filter(breed == "lockdown") %>%
  mutate(lockdown = mean > 0) %>%
  select(-c("stdev", "lower", "upper"))

data_aggr = data_aggr %>% filter(breed != "lockdown")

# set order of breeds for legend
data_long$breed <- factor(data_aggr$breed, levels=c("susceptibles",
                                                    "latents",
                                                    "asymptomatics",
                                                    "symptomatics", 
                                                    "recovereds",
                                                    "deads"))

data_aggr$breed <- factor(data_aggr$breed, levels=c("susceptibles",
                                                    "latents",
                                                    "asymptomatics",
                                                    "symptomatics", 
                                                    "recovereds",
                                                    "deads"))

# extract information for plotting
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size
num_runs = max(data_long$run_num) # num runs for ylabel
num_ticks = max(data_aggr$step) # num ticks for xaxis ticks

# lineplot of average counts, with stdev ribbon and lockdown highlighting
ggplot(data_aggr, aes(x=step, y=mean, group=breed)) + 
  geom_area(data = aggr_ld, aes(x = step, y = pop_size * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_line(aes(color=breed), size = 1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = breed), alpha=0.2) +
  coord_cartesian(ylim = c(0, pop_size)) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean count over %s runs", num_runs))

# TO-DO: find a way to add a legend clarifying lockdown colour

######### BREEDS AND LOCKDOWN AVERAGE PLOT % VERSION

# data_aggr_per = data.frame(data_aggr)
# data_aggr_per$mean = data_aggr_per$mean / pop_size * 100
# data_aggr_per$stdev = data_aggr_per$stdev / pop_size * 100
# data_aggr_per$lower = data_aggr_per$lower / pop_size * 100
# data_aggr_per$upper = data_aggr_per$upper / pop_size * 100
# 
# ggplot(data_aggr_per, aes(x=step, y=mean, group=breed)) +
#   geom_area(data = aggr_ld, aes(x = step, y = pop_size * lockdown),
#             inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
#   geom_line(aes(color=breed), size = 1) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill = breed), alpha=0.2) +
#   coord_cartesian(ylim = c(0, 100)) +
#   scale_color_brewer(palette="Set3") +
#   scale_fill_brewer(palette="Set3") +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
#   labs(x = "day", y = sprintf("percentage over %s runs", num_runs))

######### AVERAGE CONTACTS OVER TIME PLOT

# isolate contact info
contact = raw[ ,grepl("run_num|step|num_contacts", names(raw))]

# find mean and stdev over the runs
cont_aggr = contact %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts))

# add upper and lower bounds
cont_aggr$lower = cont_aggr$mean - cont_aggr$stdev
cont_aggr$upper = cont_aggr$mean + cont_aggr$stdev

# max contacts for ylim
max_cont = max(cont_aggr$mean)

# lineplot of contacts with stdev ribbon and lockdown hightlight
ggplot(cont_aggr, aes(x=step, y=mean)) +
  geom_area(data = aggr_ld, aes(x = step, y = max_cont * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_line(size = 1, color = "orange") +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill = "orange") +
  coord_cartesian(ylim = c(0, max_cont)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean contacts over %s runs", num_runs))

######### AVERAGE DEATH STATISTICS PLOT

deads = raw[ ,grepl("^dead|run_num|step", names(raw))]

deads_long = deads %>% pivot_longer (
  cols = starts_with("dead"),
  names_to = "age",
  names_prefix = "dead_",
  values_to = "count"
)

deads_long = deads_long %>%
  group_by(run_num,age) %>%
  mutate(new_deaths = c(0,diff(count)))

deads_aggr = deads_long %>%
  group_by(step, age) %>%
  summarise(mean = mean(count), stdev = sd(count))

deads_aggr$lower = deads_aggr$mean - deads_aggr$stdev
deads_aggr$upper = deads_aggr$mean + deads_aggr$stdev

# lineplot of deaths by age with stdev ribbon
ggplot(deads_aggr, aes(x=step, y=mean, group=age)) +
  geom_line(aes(color=age), size = 1) +
  geom_area(data = aggr_ld, aes(x = step, y = pop_size * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = age), alpha=0.2) +
  coord_cartesian(ylim = c(0, max(deads_aggr$upper))) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean death count by age over %s runs", num_runs))

######### NON-AGGR BREED PLOT

# non-aggregated lockdown info
ld = data_long %>% 
  filter(breed == "lockdown") %>%
  mutate(lockdown = count > 0) %>%
  select(-c("breed", "count"))

non_aggr_plot = function(line_data, lock_data, breeds) {
  ggplot(subset(line_data, breed %in% breeds),
         aes(x=step, y=count)) +
    geom_point(aes(color=breed), alpha=0.7, size = 1) +
    geom_line(data = lock_data, aes(x = step, y = (pop_size / 2) * lockdown),
              inherit.aes = FALSE, color = "grey", alpha = 0.5) +
    scale_color_brewer(palette="Set3") +
    scale_fill_brewer(palette="Set3") +
    coord_cartesian(ylim = c(0, pop_size))
}

non_aggr_plot(data_long, ld, c("deads", "recovereds", "susceptibles"))
non_aggr_plot(data_long, ld, c("latents", "symptomatics", "asymptomatics"))

ggplot(subset(data_long, breed == "susceptibles"),
       aes(x=step, y=count)) +
  geom_area(data = ld, aes(x = step, y = pop_size * lockdown, fill = as.factor(run_num)),
            inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color=breed), alpha=0.7, size = 1) +
  scale_color_brewer(palette="Set3") +
  scale_fill_manual(values = c("lightgrey","lightgrey","lightgrey","lightgrey","lightgrey",
                               "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey")) +
  coord_cartesian(ylim = c(0, pop_size))

######### NON-AGGR NEW DEATHS PLOT

ggplot(deads_long, aes(x=step, y=new_deaths)) +
  geom_point(aes(color=age), alpha=0.8, size = 1) +
  geom_line(data = ld, aes(x = step, y = max(deads_long$new_deaths) * lockdown),
            inherit.aes = FALSE, color = "grey", alpha = 0.5) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3")