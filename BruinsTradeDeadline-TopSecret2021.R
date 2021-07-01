library("readxl")
players <- read_excel("NHLPlayersData2021.xlsx")
playersOfInterest<-players%>%
  filter(height>6.0)%>%
  group_by(team,position)%>%
  mutate_at(
    vars(pims,hits,shots,blocks),
    scale
  )%>%
  mutate(
    bruinsSecretSauce=sum(pims,hits,shots,blocks)
  )%>%
  filter(bruinsSecretSauce>3)%>%
  arrange(desc(height))
View(playersOfInterest)