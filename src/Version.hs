module Version where

import Distribution.PackageDescription.TH

-- v1.0 will be all original nox msgs supported, if not game modes (like Quest)
version :: String
version = $(packageVariable (pkgVersion . package))
