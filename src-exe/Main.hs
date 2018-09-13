{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip(run, nothingAfterDownload)
import Papa

main ::
  IO ()
main =
  run nothingAfterDownload

{- todo

* write README of general flow
* test on windows

other lib
* tar download
* move symlinks to their own directory
* crop pdf files with pdfcrop --margins
* convert pdf to ps with pdftops

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}
