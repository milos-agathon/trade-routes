# 1 PACKAGES

install.packages("devtools")
devtools::install_github(
    "ropensci/comtradr@main"
)

libs <- c(
    "tidyverse", "comtradr", "giscoR",
    "sf", "sfheaders", "CoordinateCleaner"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs],
        dependencies = T
    )
}

invisible(lapply(
    libs, library,
    character.only = T
))

# 2. GET TRADE DATA
#------------------

comtradr::set_primary_comtrade_key("") # PLEASE INSERT YOUR API KEY BETWEEN THE DOUBLE QUOTATION MARKS

wheat_codes <- comtradr::ct_commodity_lookup(
    "wheat",
    return_code = F,
    return_char = T
)

wheat_exports <- comtradr::ct_get_data(
    commodity_code = "1001",
    flow_direction = "export",
    reporter = "UKR",
    partner = "all_countries",
    start_date = 2022,
    end_date = 2022
)

head(wheat_exports)

wheat_df <- wheat_exports |>
    dplyr::select(
        partner_iso,
        qty
    )

# 3. FETCH CAPITALS
#------------------

data(countryref)
head(countryref)

capitals <- countryref |>
    dplyr::filter(
        !is.na(capital)
    ) |>
    dplyr::group_by(
        iso3, capital
    ) |>
    dplyr::summarise_at(
        vars(
            capital.lon,
            capital.lat
        ), max
    ) |>
    dplyr::rename(
        long = capital.lon,
        lat = capital.lat
    )

# 4. STARTING/ENDING POINTS
#--------------------------

end_coords <- wheat_df |>
    dplyr::left_join(
        capitals,
        by = c("partner_iso" = "iso3")
    ) |>
    dplyr::select(
        partner_iso,
        qty,
        long, lat
    ) |>
    na.omit()

start_coords <- capitals |>
    dplyr::filter(
        iso3 == "UKR"
    ) |>
    dplyr::group_by(iso3) |>
    dplyr::slice(
        rep(
            1:max(nrow(end_coords)
            ),
            each = max(nrow(end_coords)
            )
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(long, lat)

# 5. GENERATE TRADE LINES
#------------------------

start_coords$linestring_id <-
end_coords$linestring_id <-
seq_len(
    nrow(
        start_coords
    )
)

wheat_lines_sf <- sfheaders::sf_linestring(
    dplyr::bind_rows(
        start_coords,
        end_coords
    ) |>
    dplyr::arrange(
        linestring_id
    ),
    x = "long",
    y = "lat",
    linestring_id = "linestring_id"
) |>
sf::st_set_crs(4326)

wheat_lines_sf <- cbind(
    wheat_lines_sf,
    end_coords[
        ,c("partner_iso", "qty")
    ]
)

# 6. WORLD SHAPEFILE
#-------------------

world_shp <- giscoR::gisco_get_countries(
    resolution = "10"
) |>
subset(
    NAME_ENGL != "Antarctica"
)

# 7. MAP
#-------

crs_robinson <- "+proj=robin +lon_0=0w"

wheat_points_sf <- end_coords |>
    dplyr::group_by(partner_iso) |>
    dplyr::arrange(
        dplyr::desc(qty)
    ) |>
    dplyr::ungroup() |>
    sf::st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    )

p1 <- ggplot() +
geom_sf(
    data = world_shp,
    fill = "#063140",
    color = "#18BFF2",
    size = .2,
    alpha = .35
) +
geom_sf(
    data = wheat_lines_sf,
    aes(
        size = qty / 1000000,
        alpha = qty / 1000000
    ),
    fill = "#ff6103",
    color = "#ff6103"
) +
geom_sf(
    data = wheat_points_sf,
    aes(
        size = qty / 1000000
    ),
    fill = "#ff6103",
    color = "#ff6103",
    alpha = .85,
    stroke = .25
) +
scale_size(
    name = "thousands of tonnes",
    range = c(.5, 2)
) +
scale_alpha(
    range = c(.25, .75)
) +
coord_sf(crs = crs_robinson) +
guides(
    alpha = "none",
    size = guide_legend(
        override.aes = list(
            fill = NULL,
            alpha = .85,
            color = "#ff6103"
        ),
        direction = "horizontal",
        keyheight = unit(1.5, "mm"),
        keywidth = unit(15, "mm"),
        title.position = "top",
        title.hjust = .5,
        label.hjust = .5,
        label.position = "top",
        nrow = 1,
        byrow = T
    )
) +
labs(
    x = "",
    y = "",
    subtitle = "",
    title = "Wheat imports from Ukraine in 2022",
    caption = "United Nations. 2023. UN comtrade
      http://comtrade.un.org"
) +
theme_void() +
theme(
    plot.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    panel.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    legend.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    legend.position = c(.55, 0),
    panel.grid.major = element_line(
        color = "#052833",
        size = 0
    ),
    plot.title = element_text(
        size = 22,
        color = "#ff6103",
        hjust = .5, vjust = 1
    ),
    plot.caption = element_text(
        size = 8,
        color = "grey80",
        hjust = .15, vjust = 0
    ),
    legend.title = element_text(
        size = 10,
        color = "#ff6103"
    ),
    legend.text = element_text(
        size = 9,
        color = "#ff6103"
    ),
    plot.margin = unit(
        c(
            t = 1, r = -2,
            b = .5, l = -2
        ), "lines"
    )
)

ggsave(
    "ukraine-export_wheat-nolabel.png",
    width = 10, h = 6, dpi = 600,
    device = "png", bg = "#052833", p1
) 

p2 <- ggplot() +
geom_sf(
    data = world_shp,
    fill = "#063140",
    color = "#18BFF2",
    size = .2,
    alpha = .35
) +
geom_sf(
    data = wheat_lines_sf,
    aes(
        size = qty / 1000000,
        alpha = qty / 1000000
    ),
    fill = "#ff6103",
    color = "#ff6103"
) +
geom_sf(
    data = wheat_points_sf,
    aes(
        size = qty / 1000000
    ),
    fill = "#ff6103",
    color = "#ff6103",
    alpha = .85,
    stroke = .25
) +
geom_sf_label(
    data = wheat_points_sf[c(1:5),], # no zero
    aes(
        label = paste0(
            partner_iso, 
            ":", "",
            round(qty / 1000000, 0))
    ),
    size = 2,
    hjust = 0,
    vjust = 1,
    color = "#ff6103",
    alpha = .85
) +
scale_size(
    name = "thousands of tonnes",
    range = c(.5, 2)
) +
scale_alpha(
    range = c(.25, .75)
) +
coord_sf(crs = crs_robinson) +
guides(
    alpha = "none",
    size = guide_legend(
        override.aes = list(
            fill = NULL,
            alpha = .85,
            color = "#ff6103"
        ),
        direction = "horizontal",
        keyheight = unit(1.5, "mm"),
        keywidth = unit(15, "mm"),
        title.position = "top",
        title.hjust = .5,
        label.hjust = .5,
        label.position = "top",
        nrow = 1,
        byrow = T
    )
) +
labs(
    x = "",
    y = "",
    subtitle = "",
    title = "Wheat imports from Ukraine in 2022",
    caption = "United Nations. 2023. UN comtrade
      http://comtrade.un.org"
) +
theme_void() +
theme(
    plot.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    panel.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    legend.background = element_rect(
        fill = "#052833",
        color = NA
    ),
    legend.position = c(.55, 0),
    panel.grid.major = element_line(
        color = "#052833",
        size = 0
    ),
    plot.title = element_text(
        size = 22,
        color = "#ff6103",
        hjust = .5, vjust = 1
    ),
    plot.caption = element_text(
        size = 8,
        color = "grey80",
        hjust = .15, vjust = 0
    ),
    legend.title = element_text(
        size = 10,
        color = "#ff6103"
    ),
    legend.text = element_text(
        size = 9,
        color = "#ff6103"
    ),
    plot.margin = unit(
        c(
            t = 1, r = -2,
            b = .5, l = -2
        ), "lines"
    )
)

ggsave(
    "ukraine-export_wheat-label.png",
    width = 10, h = 6, dpi = 600,
    device = "png", bg = "#052833", p2
)  

# PART II: MULTIPLE COUNTRIES

wheat_exports <- comtradr::ct_get_data(
    commodity_code = "1001",
    flow_direction = "export",
    reporter = c("UKR", "AUS", "USA"),
    partner = "all",
    start_date = 2022,
    end_date = 2022
)

wheat_df <- wheat_exports |>
    dplyr::select(
        reporter_iso,
        partner_iso,
        qty
    )

end_coords <- wheat_df |>
    dplyr::left_join(
        capitals,
        by = c("partner_iso" = "iso3")
    ) |>
    dplyr::select(
        reporter_iso,
        partner_iso,
        qty,
        long, lat
    ) |>
    na.omit()

start_coords <- end_coords |>
    dplyr::select(-long, -lat) |>
    dplyr::left_join(
        capitals,
        by = c("reporter_iso" = "iso3") # REPORTER_ISO
    ) |>
    dplyr::select(
        reporter_iso,
        partner_iso,
        qty, long, lat
        ) |>
    na.omit()

start_coords$linestring_id <-
end_coords$linestring_id <-
seq_len(
    nrow(
        start_coords
    )
)

wheat_lines_sf <- sfheaders::sf_linestring(
    dplyr::bind_rows(
        start_coords,
        end_coords
    ) |>
    dplyr::arrange(
        linestring_id
    ),
    x = "long",
    y = "lat",
    linestring_id = "linestring_id"
) |>
sf::st_set_crs(4326)

wheat_lines_sf <- cbind(
    wheat_lines_sf,
    end_coords[
        ,c("reporter_iso", "partner_iso", "qty")
    ]
)

cols <- c(
    "#0578aa",
    "#cb992c",
    "#bb023a"
)

p3 <- ggplot() +
geom_sf(
    data = world_shp,
    fill = "grey20",
    color = "grey60",
    size = .2
) +
geom_sf(
    data = wheat_lines_sf,
    aes(
        size = qty / 1000000,
        alpha = qty / 1000000,
        fill = reporter_iso,
    color = reporter_iso
    )  
) +
scale_size(
    name = "thousands of tonnes",
    range = c(.2, 1.5)
) +
scale_alpha(
    range = c(.2, 1)
) +
scale_color_manual(
    name = "",
    values = cols,
) +
scale_fill_manual(
    name = "",
    values = cols,
) +
coord_sf(crs = crs_robinson) +
guides(
    alpha = "none",
    color = "none",
    fill = "none",
    size = guide_legend(
        override.aes = list(
            fill = NULL,
            alpha = .85,
            color = "white"
        ),
        direction = "horizontal",
        keyheight = unit(1.5, "mm"),
        keywidth = unit(15, "mm"),
        title.position = "top",
        title.hjust = .5,
        label.hjust = .5,
        label.position = "top",
        nrow = 1,
        byrow = T
    )
) +
labs(
    x = "",
    y = "",
    subtitle = "",
    title = "Wheat imports from Ukraine in 2022",
    caption = "United Nations. 2023. UN comtrade
      http://comtrade.un.org"
) +
theme_void() +
theme(
    plot.background = element_rect(
        fill = "grey20",
        color = NA
    ),
    panel.background = element_rect(
        fill = "grey20",
        color = NA
    ),
    legend.background = element_rect(
        fill = "grey20",
        color = NA
    ),
    legend.position = c(.55, 0),
    panel.grid.major = element_line(
        color = "grey20",
        size = 0
    ),
    plot.title = element_text(
        size = 22,
        color = "white",
        hjust = .5, vjust = 1
    ),
    plot.caption = element_text(
        size = 8,
        color = "white",
        hjust = .15, vjust = 0
    ),
    legend.title = element_text(
        size = 10,
        color = "white"
    ),
    legend.text = element_text(
        size = 9,
        color = "white"
    ),
    plot.margin = unit(
        c(
            t = 1, r = -2,
            b = .5, l = -2
        ), "lines"
    )
)

ggsave(
    "multiple-export_wheat.png",
    width = 10, h = 6, dpi = 600,
    device = "png", bg = "grey20", p3
)  
