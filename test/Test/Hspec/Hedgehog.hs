{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Hedgehog
  ( describeGroup
  ) where


import Protolude

import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.QuickCheck as QuickCheck

import qualified Hedgehog
import qualified Hedgehog.Internal.Config as Hedgehog
import qualified Hedgehog.Internal.Report as Hedgehog
import qualified Hedgehog.Internal.Runner as Hedgehog
import qualified Hedgehog.Internal.Property as Hedgehog
import qualified Hedgehog.Internal.Seed as Hedgehog
import qualified Hedgehog.Internal.Source as Hedgehog


data NamedProperty
  = NamedProperty (Maybe Hedgehog.PropertyName) Hedgehog.Property


instance Hspec.Example Hedgehog.Property where
  type Arg Hedgehog.Property = ()
  evaluateExample property = Hspec.evaluateExample (NamedProperty Nothing property)


instance Hspec.Example NamedProperty where
  type Arg NamedProperty = ()
  evaluateExample (NamedProperty mbName property) params _ hspecProgress = do
    let
      config = Hedgehog.propertyConfig property
      size = Hedgehog.Size . QuickCheck.maxSize . Hspec.paramsQuickCheckArgs $ params
      test = Hedgehog.propertyTest property
      hedgehogProgress report = do
        let
          Hedgehog.TestCount numSuccess = Hedgehog.reportTests report
          Hedgehog.TestLimit numTotal = Hedgehog.propertyTestLimit config
        hspecProgress (numSuccess, numTotal)
    seed <- Hedgehog.random
    report <- Hedgehog.checkReport config size seed test hedgehogProgress
    case Hedgehog.reportStatus report of
      Hedgehog.OK -> pure Hspec.Success
      Hedgehog.GaveUp -> do
        reason <- Hedgehog.renderResult (Just Hedgehog.EnableColor) mbName report
        pure $! Hspec.Failure Nothing $! Hspec.Reason reason
      Hedgehog.Failed failure -> do
        let
          location = fmap spanToLoc $ Hedgehog.failureLocation failure
          spanToLoc span = Hspec.Location
            { Hspec.locationFile = Hedgehog.spanFile span
            , Hspec.locationLine = Hedgehog.unLineNo . Hedgehog.spanStartLine $ span
            , Hspec.locationColumn = Hedgehog.unColumnNo . Hedgehog.spanStartColumn $ span
            , Hspec.locationAccuracy = Hspec.ExactLocation
            }
        reason <- Hedgehog.renderResult (Just Hedgehog.EnableColor) mbName report
        pure $! Hspec.Failure location $! Hspec.Reason reason


describeGroup :: Hedgehog.Group -> Hspec.Spec
describeGroup (Hedgehog.Group (Hedgehog.GroupName groupName) groupProperties) =
  Hspec.describe groupName $
    forM_ groupProperties $ \ (propName@(Hedgehog.PropertyName name), property) ->
      Hspec.it name $ NamedProperty (Just propName) property
