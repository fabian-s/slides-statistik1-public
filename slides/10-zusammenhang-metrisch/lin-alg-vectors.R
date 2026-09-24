# 2D-Streudiagramm + 3D-Vektorplot der zentrierten Datenvektoren fuer die
# "Korrelation = Cosinus"-Beispielfolien; vereinfacht uebernommen aus
# lin-alg-correlation.qmd (ohne gt-Tabellenpanel, Vektorlabels x_c/y_c).
# Benoetigt: plot3D, patchwork, ggplot2, scales (via setup.R geladen).

plot_vectors <- function(
  data_matrix,
  ij = TRUE,
  ik = TRUE,
  jk = TRUE, # 2d-Ebenen als visuelle Hilfe zeichnen?
  theta = 50,
  phi = 30, # Blickwinkel 3d-Plot
  # Malreihenfolge von Ebenen ("ij", "ik", "jk") und Vektoren ("x", "y"):
  # plot3D kennt keine Verdeckung, es gilt der Painter-Algorithmus --
  # ein VOR einer (halbtransparenten) Ebene gezeichneter Pfeil erscheint
  # gedaempft "hinter" ihr. Deaktivierte Ebenen werden uebersprungen.
  reihenfolge = c("ij", "ik", "jk", "x", "y")
) {
  stopifnot(setequal(reihenfolge, c("ij", "ik", "jk", "x", "y")))
  x <- data_matrix[, 1]
  y <- data_matrix[, 2]
  x_centered <- x - mean(x)
  y_centered <- y - mean(y)
  d_centered <- cbind(x_centered, y_centered)

  df <- data.frame(
    x = x,
    y = y,
    label = c("i", "j", "k")
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(shape = 1, size = 6, fill = "lightgrey") +
    ggplot2::geom_text(label = df$label, size = 4.8) +
    ggplot2::xlim(scales::expand_range(range(df$x), mul = 0.1)) +
    ggplot2::ylim(scales::expand_range(range(df$y), mul = 0.1)) +
    ggplot2::labs(x = "x", y = "y", title = "Datenpunkte") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = "red"),
      axis.line.y = ggplot2::element_line(color = "blue"),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5)
    )

  p3d <- patchwork::wrap_elements(
    full = ~ {
      par(mar = c(0, 0, 1, 0), xaxs = "i", yaxs = "i", cex.main = 1.2)
      # Koordinatenachsen i, j, k
      plot3D::arrows3D(
        x0 = 0,
        y0 = 0,
        z0 = 0,
        x1 = 1,
        y1 = 0,
        z1 = 0,
        col = "black",
        theta = theta,
        phi = phi,
        expand = 0.8,
        xlim = range(c(d_centered[1, ], 0, 2, -2)),
        ylim = range(c(d_centered[2, ], 0, 2, -2)),
        zlim = range(c(d_centered[3, ], 0, 2, -2)),
        scale = FALSE,
        box = FALSE,
        length = 0.1,
        lwd = .5,
        main = "zentrierte Datenvektoren"
      )
      plot3D::segments3D(
        x0 = max(d_centered[1, ], 0),
        y0 = 0,
        z0 = 0,
        x1 = min(d_centered[1, ], 0),
        y1 = 0,
        z1 = 0,
        col = "black",
        lwd = 0.1,
        add = TRUE
      )
      plot3D::arrows3D(
        x0 = 0,
        y0 = 0,
        z0 = 0,
        x1 = 0,
        y1 = 1,
        z1 = 0,
        col = "black",
        length = 0.1,
        lwd = .5,
        add = TRUE
      )
      plot3D::segments3D(
        y0 = max(d_centered[2, ], 0),
        x0 = 0,
        z0 = 0,
        y1 = min(d_centered[2, ], 0),
        x1 = 0,
        z1 = 0,
        col = "black",
        lwd = 0.1,
        add = TRUE
      )
      plot3D::arrows3D(
        x0 = 0,
        y0 = 0,
        z0 = 0,
        x1 = 0,
        y1 = 0,
        z1 = 1,
        col = "black",
        length = 0.1,
        lwd = .5,
        add = TRUE
      )
      plot3D::segments3D(
        z0 = max(d_centered[3, ], 0),
        x0 = 0,
        y0 = 0,
        z1 = min(d_centered[3, ], 0),
        x1 = 0,
        y1 = 0,
        col = "black",
        lwd = 0.1,
        add = TRUE
      )
      plot3D::points3D(
        x = c(1.1, 0, 0),
        y = c(0, 1.1, 0),
        z = c(0, 0, 1.1),
        pch = c("i", "j", "k"),
        add = TRUE,
        col = "gray20",
        colvar = NULL,
        cex = 1.4
      )
      # Ebenen (visuelle Hilfe) und zentrierte Vektoren in gegebener
      # Malreihenfolge (Verdeckung via Painter-Algorithmus, s.o.)
      zeichne_ebene <- function(welche) {
        if (welche == "ij" && ij) {
          plot3D::rect3D(
            x0 = -2,
            y0 = -2,
            z0 = 0,
            x1 = 2,
            y1 = 2,
            z1 = NULL,
            col = scales::alpha("grey", .12),
            add = TRUE
          )
        }
        if (welche == "ik" && ik) {
          plot3D::rect3D(
            x0 = -2,
            y0 = 0,
            z0 = -2,
            x1 = 2,
            y1 = NULL,
            z1 = 2,
            col = scales::alpha("grey", .12),
            add = TRUE
          )
        }
        if (welche == "jk" && jk) {
          plot3D::rect3D(
            x0 = 0,
            y0 = -2,
            z0 = -2,
            x1 = NULL,
            y1 = 2,
            z1 = 2,
            col = scales::alpha("grey", .12),
            add = TRUE
          )
        }
      }
      zeichne_vektor <- function(welcher) {
        v <- if (welcher == "x") x_centered else y_centered
        clr <- if (welcher == "x") "red" else "blue"
        plot3D::arrows3D(
          x0 = 0,
          y0 = 0,
          z0 = 0,
          x1 = v[1],
          y1 = v[2],
          z1 = v[3],
          col = scales::alpha(clr, .5),
          lwd = 2,
          length = 0.1,
          add = TRUE
        )
      }
      for (element in reihenfolge) {
        if (element %in% c("ij", "ik", "jk")) {
          zeichne_ebene(element)
        } else {
          zeichne_vektor(element)
        }
      }
      plot3D::text3D(
        x = d_centered[1, ] * 1.3,
        y = d_centered[2, ] * 1.3,
        z = d_centered[3, ] * 1.3,
        labels = expression(x[c], y[c]),
        add = TRUE,
        col = c("red", "blue"),
        colvar = NULL,
        cex = 1.5
      )
      # Lote auf die Ebenen
      for (v in 1:2) {
        vec <- d_centered[, v]
        clr <- c("red", "blue")[v]
        plot3D::segments3D(
          x0 = vec[1],
          y0 = vec[2],
          z0 = vec[3],
          x1 = vec[1],
          y1 = vec[2],
          z1 = 0,
          col = clr,
          lwd = .3,
          lty = 2,
          add = TRUE
        )
        plot3D::segments3D(
          x0 = vec[1],
          y0 = vec[2],
          z0 = vec[3],
          x1 = vec[1],
          y1 = 0,
          z1 = vec[3],
          col = clr,
          lwd = .3,
          lty = 2,
          add = TRUE
        )
        plot3D::segments3D(
          x0 = vec[1],
          y0 = vec[2],
          z0 = vec[3],
          x1 = 0,
          y1 = vec[2],
          z1 = vec[3],
          col = clr,
          lwd = .3,
          lty = 2,
          add = TRUE
        )
      }
    },
    clip = TRUE
  )
  p1 + p3d + patchwork::plot_layout(widths = c(1, 1.4))
}
