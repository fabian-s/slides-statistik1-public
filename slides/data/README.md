# Vendored data

## mietspiegel2015.txt

Münchner Mietspiegel 2015 sample (3065 × 13), teaching dataset from the
Fahrmeir/Heumann Statistik textbook site.

- Original source: `http://chris.userweb.mwn.de/statistikbuch/mietspiegel2015.txt`
  — **dead (HTTP 404) as of 2026-07-24**, both http and https.
- Recovered 2026-07-24 from this repo's knitr cache
  (`slides/main_cache/beamer/02-mietspiegel-kleinkalt-prep_*`), i.e. the exact
  object all rendered slides were built from; round-trip verified with
  `all.equal()` against the cached data frame.
- Written via `write.table(m, row.names = FALSE)` (space-separated, quoted
  strings — `bez` contains spaces); read with `read.table(header = TRUE)`.
- Used by: `slides/04-zufallsvariablen-verteilungen/häufigkeiten-univariat.Rmd`,
  `slides/05-grafiken/streudiagramme.Rmd`, `slides/06-kennwerte-momente/streuungsmaße.Rmd`.
