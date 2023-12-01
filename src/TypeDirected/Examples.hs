{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}

module TypeDirected.Examples where

import qualified Data.Text as Text

import qualified Common.FGAST as A
import qualified TypeDirected.TypeDirected as TD

import qualified TypeDirected.HaskellBackend as HB

-- import Language.Haskell.Exts
-- import Language.Haskell.Exts.Syntax

third (x, y, z) = z

runExp = putStrLn $ Text.unpack $ HB.translateExp $ third $ TD.translate prog_1

decls_ex_1 =
  [ A.Type "E" (A.Struct [])
  , A.Type "F" (A.Struct [("field1", "E"), ("field2", "E")])
  , A.Type "A" (A.Iface [A.MeSpec "m1" (A.MeSig [("x", "E")] "E")])
  , A.Type "B" (A.Iface [A.MeSpec "m1" (A.MeSig [("x", "E")] "E")])
  , A.Type
      "C"
      ( A.Iface
          [ A.MeSpec "m0" (A.MeSig [("x", "E")] "E")
          , A.MeSpec "m1" (A.MeSig [("x", "E")] "E")
          , A.MeSpec "m2" (A.MeSig [("x", "E")] "E")
          ]
      )
  , A.Type
      "D"
      ( A.Iface
          [ A.MeSpec "m3" (A.MeSig [("x", "E")] "E")
          , A.MeSpec "m1" (A.MeSig [("x", "E")] "E")
          , A.MeSpec "m2" (A.MeSig [("x", "E")] "E")
          ]
      )
  , A.Method ("y", "E") (A.MeSpec "m1" (A.MeSig [("x2", "E")] "E")) undefined
  ]

prog_1 =
  ( decls_ex_1
  , let e = A.StructLit "E" []
     in A.StructLit "F" [e, e]
  )

{-
main = print =<< parseFile "Testing.hs"

main2 = do x <- parseFile "Testing.hs"
           putStrLn $ show x
-}
