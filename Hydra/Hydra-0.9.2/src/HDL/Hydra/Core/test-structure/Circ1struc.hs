-- See Circ1main.hs for explanation of how to run the example.

{-# LANGUAGE TemplateHaskell #-}

module Circ1struc where

import StrucCircuits
import Box
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Language.Haskell.TH
import Circ1src

$(transformModule declarations_Circ1src)
