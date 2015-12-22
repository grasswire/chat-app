module Handler.CommonSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "health check" $ do
        it "gives a 200" $ do
            get HealthCheckR
            statusIs 200