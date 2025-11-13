
bigmart = read_csv('_sessions/LinearModelsIII/1_Data/bigmart_en.csv') 

a  = unique(bigmart$Outlet_Identifier)
for(i in 1:length(a)){
  sel = bigmart$Outlet_Identifier == a[i]
  cor(bigmart %>% filter(sel) %>% pull(Item_Outlet_Sales),
      bigmart %>% filter(sel) %>% pull(Item_Visibility)) %>% print()
}

bigmart = bigmart %>% 
  select(Item_Outlet_Sales, Item_Fat_Content, Item_Visibility, Item_Type, Item_MRP, Outlet_Type) %>% 
  rename(Verkäufe = Item_Outlet_Sales, 
         Fettgehalt = Item_Fat_Content,
         Visibilität = Item_Visibility, 
         Typ = Item_Type, 
         Max_Preis = Item_MRP,
         Ladengrösse = Outlet_Type) %>% 
  mutate(Verkäufe = round(Verkäufe),
         Fettgehalt = case_when(
    Fettgehalt == "LF"~ "niedrig",
    Fettgehalt == "low fat"~ "niedrig",
    Fettgehalt == "Low Fat"~ "niedrig",
    Fettgehalt == "reg"~ "normal",
    Fettgehalt == "Regular"~ "normal"
  ),
  Ladengrösse = case_when(
    Ladengrösse == "Grocery Store"~ "Lebensmittelladen",
    Ladengrösse == "Supermarket Type1"~ "Supermarkt_Gr.1",
    Ladengrösse == "Supermarket Type2"~ "Supermarkt_Gr.2",
    Ladengrösse == "Supermarket Type3"~ "Supermarkt_Gr.3"
  ),
   Typ = case_when(
   Typ == "Baking Goods"~ "Backwaren",
   Typ == "Breads"~ "Brote",
   Typ == "Breakfast"~ "Frühstücksartikel",
   Typ == "Canned"~ "Dosenware",
   Typ == "Dairy"~ "Milchprodukt",
   Typ == "Frozen Foods"~ "Gefrorenes",
   Typ == "Fruits and Vegetables"~ "Früchte&Gemüse",
   Typ == "Hard Drinks"~ "Alkohol",
   Typ == "Health and Hygiene"~ "Gesundheitsartikel",
   Typ == "Household"~ "Haushaltsartikel",
   Typ == "Meat"~ "Fleisch",
   Typ == "Others"~ "Andere",
   Typ == "Seafood"~ "Fisch&Meeresfrüchte",
   Typ == "Snack Foods"~ "Snacks",
   Typ == "Soft Drinks"~ "Getränke",
   Typ == "Starchy Foods"~ "Cerealien"))

bigmart = bigmart %>%  na.omit()
write_csv(bigmart, '_sessions/LinearModelsIII/1_Data/bigmart.csv')

cat(paste0('|',names(bigmart),'||'),sep='\n')

cat(paste0('Typ == "',names(table(bigmart$Typ)),'"~ "",'),sep='\n')
cat(paste0('Typ == "',names(table(bigmart$Fettgehalt)),'"~ "",'),sep='\n')
cat(paste0('Typ == "',names(table(bigmart$Ladengrösse)),'"~ "",'),sep='\n')

tapply(bigmart$Max_Preis, bigmart$Typ, mean)

plot(bigmart$Visibilität, bigmart$Verkäufe)

sapply(bigmart, function(x) sum(is.na(x)))

summary(glm(Verkäufe ~ Visibilität, data = bigmart, family = 'poisson'))
summary(glm(Verkäufe ~ Fettgehalt*Ladengrösse, data = bigmart, family = 'poisson'))

summary(glm(round(Verkäufe) ~Visibilität +Ladengrösse, data = bigmart, family = 'poisson'))
summary(MASS::glm.nb(round(Verkäufe) ~ Ladengrösse + Visibilität, 
                     data = bigmart %>% 
                       mutate(Visibilität = skaliere(Visibilität),
                                               Max_Preis = skaliere(Max_Preis))))


hist(bigmart$Visibilität)

bigmart$Verkäufe
