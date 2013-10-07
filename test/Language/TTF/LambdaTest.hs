{-# LANGUAGE TemplateHaskell #-}

module Language.TTF.LambdaTest where

import Test.HUnit
import Test.Framework.TH
import Test.Framework.Providers.HUnit

lambdaTestGroup = $(testGroupGenerator)

case_Lambda_FreeVars = do 1 @=? 2
