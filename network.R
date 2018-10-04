list.of.packages <- c("igraph","data.table","reshape2","visNetwork")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/unicef_network")

dat = read.csv(
  "https://docs.google.com/spreadsheets/d/1ELcnWV8GQOUkHwzihVIzz3WRjIVZUQqD3lNgW_5AsBc/export?format=csv&id=1ELcnWV8GQOUkHwzihVIzz3WRjIVZUQqD3lNgW_5AsBc&gid=436638371"
  ,header=T
  )

# Filter out legacy
dat = subset(dat, Status!="Legacy")

#### Vis 1 ####
depts = unique(dat$Dept)
dept_nodes = data.frame(id=depts,name=depts,type="Department")
systems = unique(dat$Abbrev)
system_nodes = data.frame(id=systems,name=systems,type="System")
planning_node = data.frame(id="Planning",name="Planning",type="Status")
managing_node = data.frame(id="Managing",name="Managing",type="Status")
monitoring_node = data.frame(id="Monitoring",name="Monitoring",type="Status")
nodes = rbind(dept_nodes,system_nodes,planning_node,managing_node,monitoring_node)
dept_links = unique(dat[c("Abbrev","Dept")])
names(dept_links) = c("from","to")
status_links = unique(dat[c("Abbrev","Planning","Managing","Monitoring")])
status_links = melt(status_links,id.vars="Abbrev")
status_links = subset(status_links,!is.na(value))
status_links$value = NULL
names(status_links) = c("from","to")
links = rbind(dept_links,status_links)
links$weight = 1

nodes$group <- nodes$type
nodes$label = nodes$name
nodes$font.size=20
links$smooth = F
visnet1 <- visNetwork(nodes, links, height = "800px", width = "100%")
visnet1 <- visGroups(visnet1, groupname = "Department", shape = "square",
                     color = list(background = "green", border="black"))
visnet1 <- visGroups(visnet1, groupname = "System", shape = "dot",       
                     color = list(background = "tomato", border="black"))
visnet1 <- visGroups(visnet1, groupname = "Status", shape = "diamond",   
                     color = list(background = "orange", border="black"))
visnet1 %>%
  visLegend(main="Legend", position="right", ncol=1) %>%
  visOptions(manipulation = TRUE) %>% 
  visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}") %>% 
  visSave(file = "vis1.html")

#### Vis 2 ####
systems = unique(dat$Abbrev)
system_nodes = data.frame(id=systems,name=systems,type="System")
planning_node = data.frame(id="Planning",name="Planning",type="Status")
managing_node = data.frame(id="Managing",name="Managing",type="Status")
monitoring_node = data.frame(id="Monitoring",name="Monitoring",type="Status")
outcomes_node = data.frame(id="Outcomes",name="Outcomes",type="Data link")
outputs_node = data.frame(id="Outputs",name="Outputs",type="Data link")
activities_node = data.frame(id="Activities",name="Activities",type="Data link")
transactions_node = data.frame(id="Transactions",name="Transactions",type="Data link")
pidb_code_node = data.frame(id="PIDB.Codes",name="PIDB.Codes",type="Data link")
nodes = rbind(
  system_nodes,
  planning_node,
  managing_node,
  monitoring_node,
  outcomes_node,
  activities_node,
  transactions_node,
  pidb_code_node
)
status_links = unique(dat[c("Abbrev","Planning","Managing","Monitoring")])
status_links = melt(status_links,id.vars="Abbrev")
status_links = subset(status_links,!is.na(value))
status_links$value = NULL
names(status_links) = c("from","to")
data_links = unique(dat[c("Abbrev","Outcomes","Outputs","Activities","Transactions","PIDB.Codes")])
data_links = melt(data_links,id.vars="Abbrev")
data_links = subset(data_links,!is.na(value))
data_links$value = NULL
names(data_links) = c("from","to")
links = rbind(status_links, data_links)
links$weight = 1

nodes$group <- nodes$type
nodes$label = nodes$name
nodes$font.size=20
links$smooth = F
visnet2 <- visNetwork(nodes, links, height = "800px", width = "100%")
visnet2 <- visGroups(visnet2, groupname = "Data link", shape = "square",
                     color = list(background = "blue", border="black"))
visnet2 <- visGroups(visnet2, groupname = "System", shape = "dot",       
                     color = list(background = "tomato", border="black"))
visnet2 <- visGroups(visnet2, groupname = "Status", shape = "diamond",   
                     color = list(background = "orange", border="black"))
visnet2 %>%
  visLegend(main="Legend", position="right", ncol=1) %>%
  visOptions(manipulation = TRUE) %>% 
  visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}") %>% 
  visSave(file = "vis2.html")
