#' middle-side
#' 
#' @description This function permit to extract the coordinates of the middle of a segment
#'
#' @param xa abscissa of a first point, a number
#' @param ya ordinate of a first point, a number
#' @param xb abscissa of a second point, a number
#' @param yb ordinate of a second point, a number
#'
#' @return a vector (x,y), the coordinates of the middle of a segment
#' @noRd
#'
#' @examples
middle_side<-function(xa,ya,xb,yb){
  return(c((xa+xb)/2,(ya+yb)/2))
}


##########################################

#' divide_triangle
#'
#' @description this function permit to divide a triangle in four other triangles 
#' thanks to the middle of each segment. It follows the Sierpinski algorithm.
#' @param coordinates, coordinates of the three tops of the triangle
#'
#' @return a list of the coordinates of the three tops of the four triangles
#' @export
#'
#' @examples
divide_triangle<-function(coordinates=c(xa, ya, xb, yb, xc, yc)){
  #Extraction des milieu des différents segments
  coord_ab <-
    middle_side(coordinates[1], coordinates[2], coordinates[3], coordinates[4])
  coord_ac <-
    middle_side(coordinates[1], coordinates[2], coordinates[5], coordinates[6])
  coord_bc <-
    middle_side(coordinates[3], coordinates[4], coordinates[5], coordinates[6])
  # Création des nouveaux triangles grâce aux milieux des segments
  triangle_1 <-
    c(coordinates[1], coordinates[2], coord_ab[1], coord_ab[2], coord_ac[1], coord_ac[2])
  triangle_2 <-
    c(coordinates[3], coordinates[4], coord_ab[1], coord_ab[2], coord_bc[1], coord_bc[2])
  triangle_3 <-
    c(coordinates[5], coordinates[6], coord_ac[1], coord_ac[2], coord_bc[1], coord_bc[2])
  return(list(triangle_1, triangle_2, triangle_3))
}

############################################

#' divide_list_triangle
#'
#' @description  This function permit, from a list of triangles, to divide each triangles in four other triangles.
#' It follows the Sierpinski process.
#'
#' @param list_triangle, a initial list of triangle(s)
#' 
#'
#' @return another list of triangles, four times larger
#' @export
#'
#' @examples
divide_list_triangle <- function(list_triangle){ 
  sublist <- list()
  for (i in list_triangle){ 
    sublist <- append(sublist,divide_triangle(i))
  }
  return(sublist)
}

#######################################"

#' plot_triangles
#' 
#' @description This function permits to create a plot of the  different triangles that are in a list with
#' different coordinates.
#' @import ggplot2
#' @param list_triangle, a list of the coordinates of the different triangles
#'
#' @return a plot of the different triangles
#' @export
#'
#' @examples
plot_triangles <- function(list_triangle) {
  df <- data.frame()
  
  #Making of the dataframe with the coordinates of the triangles
  for (i in seq_along(list_triangle)) {
    df <- rbind(df, data.frame(
      x = c(list_triangle[[i]][1], list_triangle[[i]][3], list_triangle[[i]][5]),
      y = c(list_triangle[[i]][2], list_triangle[[i]][4], list_triangle[[i]][6]),
      triangle = i
    ))
  }
  
  #Plot of the different triangles
  plot <- ggplot(df, aes(x, y, fill = factor(triangle))) +
    geom_polygon(color = "black") +
    scale_fill_manual(values = rep("black", length(list_triangle)), guide = "none") +
    theme_void()
  
  return(plot)
}


##########################################


#' distance_points
#' 
#' @description This function calculates the distance between two different points
#' @param x1, a number, an abscissa of a first point 
#' @param y1, a number, an ordinate of a first point
#' @param x2, a number, an abscissa of a second point
#' @param y2, a number, an ordinate of a second point
#'
#' @return the distance between two points
#' @export
#'
#' @examples
distance_points <- function(x1, y1, x2, y2) {
  return(sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2))
}

############################################

#' area_list
#'
#' @description This function permits to calculate a list of the area of the different triangles but also the sum 
#' of all triangles.
#' @param list_triangle, a list of the different coordinates of different triangles
#' @import heron
#' @return a list of of the area of the different triangles and also the sum of all the triangles.
#' @export
#'
#' @examples
area_list <- function(list_triangle) {
  sum_area <- 0
  # Distance between the different tops
  for (i in seq_along(list_triangle)) {
    A_B <-
      distance_points(list_triangle[[i]][1],
                    list_triangle[[i]][2],
                    list_triangle[[i]][3],
                    list_triangle[[i]][4])
    B_C <-
      distance_points(list_triangle[[i]][3],
                    list_triangle[[i]][4],
                    list_triangle[[i]][5],
                    list_triangle[[i]][6])
    C_A <-
      distance_points(list_triangle[[i]][5],
                    list_triangle[[i]][6],
                    list_triangle[[i]][1],
                    list_triangle[[i]][2])
    
    # On calcule l'air des triangles 
    sum_area <- sum_area + heron(A_B, B_C, C_A)
  }
  return(sum_area)
}