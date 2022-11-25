####################################################################
#' Print lares R library Logo
#'
#' Used "ASCII Art Generator" from manytools.org to convert logo to ASCII.
#' \href{https://manytools.org/hacker-tools/convert-images-to-ascii-art/}{Visit}.
#'
#' @param version Boolean. Include R and lares version?
#' @examples
#' lares_logo()
#' @export
lares_logo <- function(version = TRUE) {
  cat(paste("
                            (((((
                 *     /(((((((((((((*     (
                   (((/              /((((
        /     .(                           (((
    .     ((                                  *(((/     (
      (((                                        ((((((
   ,(((((((((                                     (((((((
   ,((((((((((((       (((((/**((((((              *(((((
   ,(((((((((((((((((,                ((            (((((
   ,((((((((((((((((    ,((((((((       ((          *((((
   ,###################            #*     #/        ,####
   *################.               .#     #        #####
   *##############                   (######        #####
   *###########,   .#####,            ##           ######
   *#########    ###########          ##          #######
   *#######    ###  ,#####           ###         ########
   *#############                    ##.        #########
   *###########                      ##        ##########
    ##########                       ##       ######*,###
  ,     ####               ####      ###     ####  #(     /
      (     .#         /%%%%%%%%%%%%%%%%#    %%%      @
                 %%%%%%%%%%%%%%%%%%%%%%%%%
               /     #%%%%%%%%%%%%%%%%%*     /
                          %%%%%%%%%
                              %
                            /   %
"))
  if (version) {
    temp <- packageDescription("lares")
    ver <- paste(ifelse(is.null(temp$Repository), "dev", "stable"),
      temp$Version,
      sep = "-"
    )
    cat(paste("\nlares:", ver, "\n"))
    cat(R.version$version.string)
  }
}
