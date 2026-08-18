# Use 80-column Source Line Limit

Status: accepted

qualtdict will use an 80-column line limit for source files covered by linting,
including `R/`, `tests/`, `README.Rmd`, and vignette R Markdown sources. This
aligns the repository style policy with `goodpractice`'s line-length audit
rather than keeping a separate 120-column lint policy. Generated files such as
`man/*.Rd` and rendered `README.md` are controlled through their sources and are
not themselves the style-policy target.
