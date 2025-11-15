# New Line Feed for Long Strings (Wrapper)

Add a break or new line without breaking words. Automatically, the
function can detect your plot's width and will dynamically set an auto
width. You can adjust the relation (rel) parameter for different fonts
and sizes until perfect harmony found. Quite similar to
[`stringr::str_wrap`](https://stringr.tidyverse.org/reference/str_wrap.html)
but, if the text vector is a factor, the levels will be kept in order
and transformed.

## Usage

``` r
autoline(text, top = "auto", rel = 9)
```

## Arguments

- text:

  Character or factor vector.

- top:

  Integer. How many characters aprox. should be on each line?

- rel:

  Numeric. Relation of pixels and characters per line

## Value

Character. String (vector) including some `\n` within.

## See also

Other Tools:
[`bind_files()`](https://laresbernardo.github.io/lares/reference/bind_files.md),
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`chr2num()`](https://laresbernardo.github.io/lares/reference/chr2num.md),
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`dont_sleep()`](https://laresbernardo.github.io/lares/reference/dont_sleep.md),
[`export_plot()`](https://laresbernardo.github.io/lares/reference/export_plot.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`files_functions()`](https://laresbernardo.github.io/lares/reference/files_functions.md),
[`font_exists()`](https://laresbernardo.github.io/lares/reference/font_exists.md),
[`formatColoured()`](https://laresbernardo.github.io/lares/reference/formatColoured.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`glued()`](https://laresbernardo.github.io/lares/reference/glued.md),
[`grepm()`](https://laresbernardo.github.io/lares/reference/grepm.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`haveInternet()`](https://laresbernardo.github.io/lares/reference/haveInternet.md),
[`image_metadata()`](https://laresbernardo.github.io/lares/reference/image_metadata.md),
[`importxlsx()`](https://laresbernardo.github.io/lares/reference/importxlsx.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`json2vector()`](https://laresbernardo.github.io/lares/reference/json2vector.md),
[`list_cats()`](https://laresbernardo.github.io/lares/reference/list_cats.md),
[`listfiles()`](https://laresbernardo.github.io/lares/reference/listfiles.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`markdown2df()`](https://laresbernardo.github.io/lares/reference/markdown2df.md),
[`move_files()`](https://laresbernardo.github.io/lares/reference/move_files.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md),
[`myip()`](https://laresbernardo.github.io/lares/reference/myip.md),
[`quiet()`](https://laresbernardo.github.io/lares/reference/quiet.md),
[`read.file()`](https://laresbernardo.github.io/lares/reference/read.file.md),
[`statusbar()`](https://laresbernardo.github.io/lares/reference/statusbar.md),
[`tic()`](https://laresbernardo.github.io/lares/reference/tic.md),
[`try_require()`](https://laresbernardo.github.io/lares/reference/try_require.md),
[`updateLares()`](https://laresbernardo.github.io/lares/reference/updateLares.md),
[`warnifnot()`](https://laresbernardo.github.io/lares/reference/warnifnot.md),
[`what_size()`](https://laresbernardo.github.io/lares/reference/what_size.md)

## Examples

``` r
cat(autoline("This is a long text that may not fit into a single line", 8))
#> This is
#> a long
#> text
#> that may
#> not fit
#> into a
#> single
#> line

text <- factor(c("First value", "Second value", "First value"),
  levels = c("First value", "Second value")
)
autoline(text, 1)
#> [1] First\nvalue  Second\nvalue First\nvalue 
#> Levels: First\nvalue Second\nvalue

path <- file.path(R.home("doc"), "THANKS")
text <- paste(readLines(path), collapse = " ")
cat(autoline(text))
#> R would not be what it is today without the
#> invaluable help of these people outside of the
#> (former and current) R Core team, who contributed by
#> donating code, bug fixes and documentation: Valerio
#> Aimale, Suharto Anggono, Thomas Baier, Gabe Becker,
#> Henrik Bengtsson, Roger Bivand, Ben Bolker, David
#> Brahm, G"oran Brostr"om, Patrick Burns, Vince Carey,
#> Saikat DebRoy, Matt Dowle, Brian D'Urso, Lyndon
#> Drake, Dirk Eddelbuettel, Claus Ekstrom, Sebastian
#> Fischmeister, John Fox, Paul Gilbert, Yu Gong, Gabor
#> Grothendieck, Frank E Harrell Jr, Peter M. Haverty,
#> Torsten Hothorn, Robert King, Kjetil Kjernsmo,
#> Roger Koenker, Philippe Lambert, Jan de Leeuw,
#> Jim Lindsey, Patrick Lindsey, Catherine Loader,
#> Gordon Maclean, Arni Magnusson, John Maindonald,
#> David Meyer, Ei-ji Nakama, Jens Oehlschl"agel,
#> Steve Oncley, Richard O'Keefe, Hubert Palme, Roger
#> D. Peng, Jose' C. Pinheiro, Tony Plate, Anthony
#> Rossini, Jonathan Rougier, Petr Savicky, Guenther
#> Sawitzki, Marc Schwartz, Arun Srinivasan, Detlef
#> Steuer, Bill Simpson, Gordon Smyth, Adrian Trapletti,
#> Terry Therneau, Rolf Turner, Bill Venables, Gregory
#> R. Warnes, Andreas Weingessel, Morten Welinder,
#> James Wettenhall, Simon Wood, and Achim Zeileis.
#> Others have written code that has been adopted by
#> R and is acknowledged in the code files, including
#> J. D. Beasley, David J. Best, Richard Brent, Kevin
#> Buhr, Michael A. Covington, Bill Cleveland, Robert
#> Cleveland, G. W. Cran, C. G. Ding, Ulrich Drepper,
#> Paul Eggert, J. O. Evans, David M. Gay, H. Frick,
#> G. W. Hill, Richard H. Jones, Eric Grosse, Shelby
#> Haberman, Bruno Haible, John Hartigan, Andrew Harvey,
#> Trevor Hastie, Min Long Lam, George Marsaglia, K.
#> J. Martin, Gordon Matzigkeit, C. R. Mckenzie, Jean
#> McRae, Cyrus Mehta, Fionn Murtagh, John C. Nash,
#> Finbarr O'Sullivan, R. E. Odeh, William Patefield,
#> Nitin Patel, Alan Richardson, D. E. Roberts, Patrick
#> Royston, Russell Lenth, Ming-Jen Shyu, Richard C.
#> Singleton, S. G. Springer, Supoj Sutanthavibul,
#> Irma Terpenning, G. E. Thomas, Rob Tibshirani,
#> Wai Wan Tsang, Berwin Turlach, Gary V. Vaughan,
#> Michael Wichura, Jingbo Wang, M. A. Wong, and the
#> Free Software Foundation (for autoconf code and
#> utilities). See also files under src/extras. Many
#> more, too numerous to mention here, have contributed
#> by sending bug reports and suggesting various
#> improvements. Simon Davies whilst at the University
#> of Auckland wrote the original version of glm().
#> Julian Harris and Wing Kwong (Tiki) Wan whilst at
#> the University of Auckland assisted Ross Ihaka with
#> the original Macintosh port. R was inspired by the S
#> environment which has been principally developed by
#> John Chambers, with substantial input from Douglas
#> Bates, Rick Becker, Bill Cleveland, Trevor Hastie,
#> Daryl Pregibon and Allan Wilks. A special debt is
#> owed to John Chambers who has graciously contributed
#> advice and encouragement in the early days of R and
#> later became a member of the core team. Stefano Iacus
#> (up to 2014, a former member of R Core) and Simon
#> Urbanek developed the macOS port, including the R.app
#> GUI, toolchains and packaging. The Windows port was
#> originally developed by Guido Masarotto (for a while
#> a member of R Core) and Brian Ripley, then further
#> by Duncan Murdoch (a former member of R Core) and
#> then Jeroen Ooms (base) and Uwe Ligges (packages).
#> Tomas Kalibera is the current main developer of the
#> Windows port and provides assistance with package
#> porting. Tomas Kalibera's work has been sponsored
#> by Jan Vitek and funded by his European Research
#> Council grant "Evolving Language Ecosystems (ELE)".
#> Computing support (including hardware, hosting and
#> infrastructure) has been provided/funded by the R
#> Foundation, employers of R-Core members (notably
#> WU Wien, ETH Zurich, U Oxford and U Iowa) and by
#> Northeastern University and the University of Kent.
#> Distributions of R contain the recommended packages,
#> whose authors/contributors are listed in their
#> DESCRIPTION files.
```
