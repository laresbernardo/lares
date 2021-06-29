####################################################################
#' Generative Art: Sphere XmodY
#'
#' @family rtistry
#' @param eye,pal,var Parameters to change aesthetics and calculations
#' @return ggplot object
#' @export
rtistry_sphere <- function(eye = c(100, 0, 0),
                           pal = "auto",
                           var = 3) {
  try_require("threed")

  if (length(eye) != 3) stop("eye must be length 3")
  if (pal[1] == "auto") pal <- c("#264653", "#2a9d8f")
  if (length(pal) <= 1) stop("pal must be length >= 2")

  mesh3dobj$sphere %>%
    transform_by(invert_matrix(
      look_at_matrix(eye = eye, at = c(0, 0, 0))
    )) %>%
    perspective_projection() %>%
    as.data.frame() %>%
    mutate(aux = .data$element_id %% var == 1) %>%
    rowwise() %>%
    mutate(color = ifelse(.data$aux, pal[1], NA)) %>%
    # select(element_id, x, y, color) %>%
    ggplot(aes(.data$x, .data$y, group = .data$element_id)) +
    geom_polygon(aes(fill = .data$color), colour = "transparent") +
    # scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
    scale_fill_identity() +
    coord_equal() +
    theme_lares(clean = TRUE, legend = FALSE, background = pal[2])
}

# mesh3dobj$dodecahedron %>%
#   transform_by(invert_matrix(
#     look_at_matrix(eye = c(10,10,10), at = c(0, 0, 0))
#   )) %>%
#   perspective_projection() %>%
#   as.data.frame() %>%
#   mutate(aux = .data$element_id %% 3 == 1) %>%
#   rowwise() %>%
#   filter(aux) %>%
#   mutate(color = ifelse(.data$aux, sample(lares::lares_pal("col")[1:9])[1], NA)) %>%
#   ggplot(aes(group = .data$element_id)) +
#   geom_polygon(aes(-x, y, fill = rev(color)), color = "black", size = 0.1) +
#   coord_equal() + scale_fill_identity() +
#   theme_lares(clean = TRUE, legend = FALSE)
