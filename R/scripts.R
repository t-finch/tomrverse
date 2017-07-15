#' Custom ggplot Theme
#'
#' Modifies theme of ggplot object. White background with grey major grid lines and easily moveable legend.
#' @param base_size Number specifying base font size
#' @param legend_position Character specifying legend position (\code{"topleft"} or \code{"topright"} or \code{"lowleft"} or \code{"lowright"} or \code{"topmid"} or \code{"lowmid"})
#' @return Returns a ggplot theme
#' @export

ggtheme <- function(base_size = 8, legend_position = "default") {
          # Get legend.position and legend.justification arguments
          if(legend_position == "default")  {pos = "right"; just = "center"}
          if(legend_position == "topleft")  {pos = c(0, 1);  just = c(0, 1)}
          if(legend_position == "topright") {pos = c(1, 1);  just = c(1, 1)}
          if(legend_position == "lowleft")  {pos = c(0, 0);  just = c(0, 0)}
          if(legend_position == "lowright") {pos = c(1, 0);  just = c(1, 0)}
          if(legend_position == "topmid")   {pos = c(0.5, 1); just = c(0.5, 1)}
          if(legend_position == "lowmid")   {pos = c(0.5, 0); just = c(0.5, 0)}

          # Modify theme_grey
          theme_grey(base_size = base_size, base_family = "") %+replace%
                    theme(legend.key           = element_rect(colour = "grey90"),
                          panel.background     = element_rect(fill = "white", colour = NA),
                          panel.border         = element_rect(fill = NA, colour = 1),
                          panel.grid.major     = element_line(colour = "grey90", size = 0.2),
                          strip.background     = element_rect(fill = "grey90", colour = 1, size = 0.2),
                          axis.text            = element_text(size = base_size),
                          axis.ticks           = element_line(colour = "black", size = 0.2),
                          plot.title           = element_text(size = base_size + 8, hjust = 0, face = "bold", margin = margin(b = 3, unit = "pt")),
                          plot.subtitle        = element_text(size = base_size + 4, hjust = 0, margin = margin(b = 3, unit = "pt")),
                          strip.text           = element_text(size = base_size),
                          legend.text          = element_text(size = base_size - 1),
                          legend.title	       = element_text(size = base_size + 2),
                          legend.key.size      = unit(3, "mm"),
                          legend.position      = pos,
                          legend.justification = just,
                          legend.background    = element_rect(colour = 1, fill = scales::alpha("white", 0.9), size = 0.25)
                    )
}


#' Package sets
#'
#' Loads some useful package sets, fixes some obvious name clashes
#' @param package_set Character string specifying set of packages to load (\code{"spatial"} or \code{"stats"} or \code{"all"})
#' @return Loads packages, fixes some obvious name clashes
#' @details
#' Spatial packages are \code{raster}, \code{sp}, \code{rgdal}, \code{maptools}, \code{mapproj}, \code{geosphere}, \code{rgeos} and \code{spdplyr}.
#' The \code{raster:::select} function is overwritten with \code{dplyr:::select}.\cr\cr
#' Stats packages are \code{lme4} and \code{MuMIn}
#' @export

package_set <- function(pkgs = "all"){
          library(install.load)
          if("all" %in% pkgs){pkgs <- c("spatial", "stats")}

          ## Spatial packages
          if("spatial" %in% pkgs){
                    install_load("raster",
                                 "sp",
                                 "rgdal",
                                 "maptools",
                                 "mapproj",
                                 "geosphere",
                                 "rgeos",
                                 "spdplyr")

                    # Fix raster / dplyr clash
                    assign("select", dplyr:::select, envir = globalenv())

                    # Define coordinate reference systems
                    bng   <<- CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs')
                    mrc   <<- CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs')
                    wgs84 <<- CRS('+proj=longlat +datum=WGS84')

          }

          ## Stats packages
          if("stats" %in% pkgs){
                    install_load("lme4",
                                 "MuMIn")
          }
}
