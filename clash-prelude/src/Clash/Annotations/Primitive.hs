{-|
Copyright  :  (C) 2017-2019, Myrtle Software
                  2022,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Instruct the Clash compiler to look for primitive HDL templates provided inline
or in a specified directory. For distribution of new packages with primitive
HDL templates. Primitive guards can be added to warn on instantiating
primitives.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Annotations.Primitive
  ( dontTranslate
  , hasBlackBox
  , warnNonSynthesizable
  , warnAlways
  , Primitive(..)
  , HDL(..)
  , PrimitiveGuard(..)
  , PrimitiveWarning(..)
  , extractPrim
  , extractWarnings
  ) where

import           Control.DeepSeq                          (NFData)
import           Data.Binary                              (Binary)
import           Data.Data
import           Data.Hashable                            (Hashable)
import           GHC.Generics                             (Generic)


-- The commented code directly below this comment is affected by an old
-- GHC bug: https://ghc.haskell.org/trac/ghc/ticket/5463. In short, NOINLINE
-- pragmas generated by Template Haskell, get ignored. We'd still like a better
-- API than manually having to write all the guard/inline pragmas some day,
-- so I'm leaving the code in for now.

{-

guard :: TH.Exp -> TH.Name -> TH.Q [TH.Dec]
guard guardExpr fName =
  pure
    [ TH.PragmaD (TH.InlineP fName TH.NoInline TH.FunLike TH.AllPhases)
    , TH.PragmaD (TH.AnnP (TH.ValueAnnotation fName) (TH.SigE guardExpr typ))
    ]
  where
    typ = TH.AppT (TH.ConT ''PrimitiveGuard) (TH.TupleT 0)

applyUnit :: TH.Exp -> TH.Exp
applyUnit e = TH.AppE e (TH.TupE [])

-- | Mark a function as having a primitive. Clash will yield an error if it
-- needs to translate this function, but no blackbox was loaded. Usage:
--
-- @
-- $(hasBlackBox 'f)
-- @
--
-- If you don't want to use TemplateHaskell, add these annotations:
--
-- @
-- {-# NOINLINE f #-}
-- {-# ANN f (HasBlackBox ()) #-}
-- @
--
hasBlackBox :: TH.Name -> TH.Q [TH.Dec]
hasBlackBox = guard (applyUnit (TH.ConE 'HasBlackBox))

-- | Mark a function as non translatable. Clash will yield an error if
-- it needs to translate this function. Usage:
--
-- @
-- $(dontTranslate 'f)
-- @
--
-- If you don't want to use TemplateHaskell, add these annotations:
--
-- @
-- {-# NOINLINE f #-}
-- {-# ANN f DontTranslate #-}
-- @
--
dontTranslate :: TH.Name -> TH.Q [TH.Dec]
dontTranslate = guard (TH.ConE 'DontTranslate)

-- | Mark a function as non synthesizable. Clash will emit the given warning
-- if instantiated outside of a testbench context. Usage:
--
-- @
-- $(warnNonSynthesizable 'f "Tread carefully, user!")
-- @
--
-- If you don't want to use TemplateHaskell, add these annotations:
--
-- @
-- {-# NOINLINE f #-}
-- {-# ANN f (WarnNonSynthesizable "Tread carefully, user!" ()) #-}
-- @
--
warnNotSynthesizable :: TH.Name -> String -> TH.Q [TH.Dec]
warnNotSynthesizable nm warning =
  guard
    (applyUnit
      (TH.AppE
        (TH.ConE 'WarnNonSynthesizable)
        (TH.LitE (TH.StringL warning))))
    nm

-- | Emit warning when translating this value.
--
-- @
-- $(warnAlways 'f "Tread carefully, user!")
-- @
--
-- If you don't want to use TemplateHaskell, add these annotations:
--
-- @
-- {-# NOINLINE f #-}
-- {-# ANN f (WarnAlways "Tread carefully, user!" ()) #-}
-- @
--
warnAlways :: TH.Name -> String -> TH.Q [TH.Dec]
warnAlways nm warning =
  guard
    (applyUnit
      (TH.AppE
        (TH.ConE 'WarnAlways)
        (TH.LitE (TH.StringL warning))))
    nm
-}

-- | Marks value as not translatable. Clash will error if it finds a blackbox
-- definition for it, or when it is forced to translate it. You can annotate a
-- variable or function @f@ like:
--
-- @
-- {\-\# ANN f dontTranslate \#-\}
-- @
dontTranslate :: PrimitiveGuard ()
dontTranslate = DontTranslate

-- | Marks a value as having a blackbox. Clash will error if it hasn't found
-- a blackbox. You can annotate a variable or function @f@ like:
--
-- @
-- {\-\# ANN f hasBlackBox \#-\}
-- @
hasBlackBox :: PrimitiveGuard ()
hasBlackBox = HasBlackBox [] ()

-- | Marks value as non-synthesizable. This will trigger a warning if
-- instantiated in a non-testbench context. You can annotate a variable or
-- function @f@ like:
--
-- @
-- {\-\# ANN f (warnNonSynthesizable "Tread carefully, user!") \#-\}
-- @
--
-- Implies `hasBlackBox`.
warnNonSynthesizable :: String -> PrimitiveGuard ()
warnNonSynthesizable s = HasBlackBox [WarnNonSynthesizable s] ()

-- | Always emit warning upon primitive instantiation. You can annotate a
-- variable or function @f@ like:
--
-- @
-- {\-\# ANN f (warnAlways "Tread carefully, user!") \#-\}
-- @
--
-- Implies `hasBlackBox`.
warnAlways :: String -> PrimitiveGuard ()
warnAlways s = HasBlackBox [WarnAlways s] ()

-- | A compilation target HDL.
data HDL
  = SystemVerilog
  | Verilog
  | VHDL
  deriving (Eq, Show, Read, Data, Generic, NFData, Hashable, Enum, Bounded)

-- | The 'Primitive' constructor instructs the clash compiler to look for primitive
-- HDL templates in the indicated directory. 'InlinePrimitive' is equivalent but
-- provides the HDL template inline. They are intended for the distribution of
-- new packages with primitive HDL templates.
--
-- === Example of 'Primitive'
--
-- You have some existing IP written in one of HDLs supported by Clash, and
-- you want to distribute some bindings so that the IP can be easily instantiated
-- from Clash.
--
-- You create a package which has a @myfancyip.cabal@ file with the following stanza:
--
-- @
-- data-files: path\/to\/MyFancyIP.primitives
-- cpp-options: -DCABAL
-- @
--
-- and a @MyFancyIP.hs@ module with the simulation definition and primitive.
--
-- @
-- module MyFancyIP where
--
-- import Clash.Prelude
--
-- myFancyIP :: ...
-- myFancyIP = ...
-- {\-\# NOINLINE myFancyIP \#-\}
-- @
--
-- The @NOINLINE@ pragma is needed so that GHC will never inline the definition.
--
-- Now you need to add the following imports and @ANN@ pragma:
--
-- @
-- \#ifdef CABAL
-- import           Clash.Annotations.Primitive
-- import           System.FilePath
-- import qualified Paths_myfancyip
-- import           System.IO.Unsafe
--
-- {\-\# ANN module (Primitive [VHDL] (unsafePerformIO Paths_myfancyip.getDataDir \<\/\> "path" \<\/\> "to")) \#-\}
-- \#endif
-- @
--
-- Add more files to the @data-files@ stanza in your @.cabal@ files and more
-- @ANN@ pragma's if you want to add more primitive templates for other HDLs
--
-- === Example of 'InlineYamlPrimitive'
--
-- The following example shows off an inline HDL primitive template. It uses the
-- [interpolate](https://hackage.haskell.org/package/interpolate) package for
-- nicer multiline strings.
--
-- @
-- {\-\# LANGUAGE QuasiQuotes \#-\}
-- module InlinePrimitive where
--
-- import           Clash.Annotations.Primitive
-- import           Clash.Prelude
-- import           Data.String.Interpolate      (i)
-- import           Data.String.Interpolate.Util (unindent)
--
-- {\-\# ANN example (InlineYamlPrimitive [VHDL] $ unindent [i|
--  BlackBox:
--    kind: Declaration
--    name: InlinePrimitive.example
--    template: |-
--      -- begin InlinePrimitive example:
--      ~GENSYM[example][0] : block
--      ~RESULT <= 1 + ~ARG[0];
--      end block;
--      end InlinePrimitive example
-- |]) \#-\}
-- {\-\# NOINLINE example \#-\}
-- example :: Signal System (BitVector 2) -> Signal System (BitVector 2)
-- example = fmap succ
-- @
data Primitive
  = Primitive [HDL] FilePath
  -- ^ Description of a primitive for a given 'HDL's in a file at 'FilePath'
  | InlinePrimitive [HDL] String
  -- ^ Description of a primitive for a given 'HDL's as an inline JSON 'String'
  | InlineYamlPrimitive [HDL] String
  -- ^ Description of a primitive for a given 'HDL's as an inline YAML 'String'
  deriving (Show, Read, Data, Generic, NFData, Hashable, Eq)

-- | Primitive guard to mark a value as either not translatable or as having a
-- blackbox with an optional extra warning. Helps Clash generate better error
-- messages.
--
-- For use, see 'dontTranslate', 'hasBlackBox', 'warnNonSynthesizable' and
-- 'warnAlways'.
data PrimitiveGuard a
  = DontTranslate
  -- ^ Marks value as not translatable. Clash will error if it finds a blackbox
  -- definition for it, or when it is forced to translate it.
  | HasBlackBox [PrimitiveWarning] a
  -- ^ Marks a value as having a blackbox. Clash will error if it hasn't found
  -- a blackbox.
  deriving
    ( Show, Read, Data, Generic, NFData, Hashable, Functor, Foldable
    , Traversable, Binary, Eq )

-- | Warning that will be emitted on instantiating a guarded value.
data PrimitiveWarning
  = WarnNonSynthesizable String
  -- ^ Marks value as non-synthesizable. This will trigger a warning if
  -- instantiated in a non-testbench context.
  | WarnAlways String
  -- ^ Always emit warning upon primitive instantiation.
    deriving (Show, Read, Data, Generic, NFData, Hashable, Binary, Eq)

-- | Extract primitive definition from a PrimitiveGuard. Will yield Nothing
-- for guards of value 'DontTranslate'.
extractPrim
  :: PrimitiveGuard a
  -> Maybe a
extractPrim =
  \case
    HasBlackBox _ p -> Just p
    DontTranslate   -> Nothing

-- | Extract primitive warnings from a PrimitiveGuard. Will yield an empty list
-- for guards of value 'DontTranslate'.
extractWarnings
  :: PrimitiveGuard a
  -> [PrimitiveWarning]
extractWarnings =
  \case
    HasBlackBox w _ -> w
    DontTranslate   -> []
