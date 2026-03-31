#' Penguen Gaga Uzunluğu Grafiği
#'
#' Penguen türlerine göre gaga uzunluğu dağılımını gösterir.
#'
#' @param tur Karakter. Tür filtresi (opsiyonel).
#' @return ggplot nesnesi
#' @importFrom rlang .data
#' @examples
#' # Tüm penguenler için grafik
#' penguen_grafik()
#'
#' # Sadece Adelie türü için
#' penguen_grafik("Adelie")
#' @export
penguen_grafik <- function(tur = NULL) {

  if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
    stop("palmerpenguins paketi gerekli.")
  }

  veri <- palmerpenguins::penguins

  if (!is.null(tur)) {
    veri <- dplyr::filter(veri, .data[["species"]] == tur)
  }

  ggplot2::ggplot(
    veri,
    ggplot2::aes(
      x = .data[["bill_length_mm"]],
      fill = .data[["species"]]
    )
  ) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::theme_minimal()
} 
