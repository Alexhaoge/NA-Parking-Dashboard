th = theme_classic(base_size = 15)

plot_leaflet = function(data.selected) {
  pal <- colorNumeric(
    palette = "Reds",
    domain = parking$AvgTimeToPark)
  leaflet(data.selected) %>%
    addTiles() %>%
    #addProviderTiles(providers$CartoDB.Positron) %>%
    # addCircleMarkers(
    #   ~Longitude, ~Latitude,
    #   label = ~Geohash, radius=0.2,
    #   color = ~pal(AvgTimeToPark)) %>%
    addRectangles(
      lat1=~Latitude_SW, lng1=~Longitude_SW,
      lat2=~Latitude_NE, lng2=~Longitude_NE,
      color = 'grey', opacity = 0.2, fillOpacity = 0.7,
      fill = TRUE,  fillColor = ~pal(AvgTimeToPark),
      popup = ~paste0(
        "Avg Parking Time: ", AvgTimeToPark, "<br>",
        "Location: (", round(Longitude, 2), ",", round(Latitude, 2), ")<br>",
        City, ", ", State
      )
    ) %>% 
    addLegend("bottomleft", pal = pal, values = parking$AvgTimeToPark,
                        title = "Avg Park Time",
                        labFormat = labelFormat(suffix = "min")
              )
}

plot_krig = function(krig.output) {
  ggplot(data=map_states, aes(x=long,y=lat)) + 
    geom_tile(data=krig.output,aes(fill=var1.pred)) +
    geom_path(aes(group=group),colour = "grey40", size=1) +
    scale_fill_gradient(low="#FEEBE2", high="#7A0177") +
    coord_equal() +
    guides(fill=guide_legend(title="How difficult to park")) +
    theme(legend.position = "bottom", plot.margin=margin())
}

plot_krig_new = function(krig.output, thre) {
  pal <- colorNumeric(
    palette = "Reds",
    domain = krig.output$`var1.pred`)
  krig.output %>%
    filter(var1.pred >= thre) %>%
    leaflet() %>%
    addTiles() %>%
    #addProviderTiles(providers$CartoDB.Positron) %>%
    addRectangles(
      lat1=~lat-0.05, lng1=~long-0.05,
      lat2=~lat+0.05, lng2=~long+0.05,
      stroke = FALSE, fillOpacity = 0.01,
      fill = TRUE,  fillColor = ~pal(var1.pred),
      popup = ~paste0(
        "Avg Parking Time: ", var1.pred, "<br>",
        "Location: (", long, ",", lat, ")"
      )
    )
    # addLegend("bottomleft", pal = pal, values = krig.output$`var1.pred`,
    #           title = "Avg Park Time",
    #           labFormat = labelFormat(suffix = "min")
    # )
}

plot_bar_top = function(data.selected, var, n=15) {
  var.quoso = sym(var)
  p_big <- data.selected %>%
    slice_max(POPESTIMATE2020, n=n) %>%
    ggplot() +
    geom_bar(
      aes(y = reorder(County, POPESTIMATE2020), x = !!var.quoso),
      stat = "identity")+
    labs(x = var,
         y = "County",
         title = paste(var, "by County"),
         subtitle = "15 Most Populated counties")+th
  
  p_small <- data.selected %>%
    slice_min(POPESTIMATE2020, n=n) %>%
    ggplot()+
    geom_bar(
      aes(y = reorder(County, POPESTIMATE2020), x = !!var.quoso),
      stat = "identity")+
    labs(x = var,
         y = "County",
         subtitle = "15 Least Populated counties")+th
  
  p_big/p_small+
    plot_layout(guides="collect")
}

plot_scatter = function(data.selected, var) {
  var.quoso = sym(var)
  ggplot(data.selected)+
    geom_point(aes(x = !!var.quoso, y=POPESTIMATE2020, alpha = PercentSearching))+
    geom_text_repel(aes(x=!!var.quoso, y=POPESTIMATE2020, label=County))+
    labs(x = var,
         y = "Population Estimate (by county 2020)",
         title = paste0("Population vs. ", var),
         alpha = "Percent Searching",
         subtitle = "By County, Opacity by Percentage of Drivers Searching for Parking")+
    scale_y_continuous(label=comma)
}


plot_hist = function(selected.state, selected.city, var) {
  var.quoso = sym(var)
  selected_data = parking
  if (selected.city != "All cities in this state")
    selected_data = filter(selected_data, City == selected.city)
  if (selected.state != "All states")
    selected_data = filter(selected_data, State == selected.state)
  ggplot(selected_data)+
    geom_histogram(aes(!!var.quoso)) +
    labs(x = var,
        title = paste("Histogram of", var),
        alpha = "Percent Searching",
        subtitle = paste(selected.city, selected.state, sep = ",")) +
    scale_y_continuous(label=comma)
}
