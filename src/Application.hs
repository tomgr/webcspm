{-# LANGUAGE TemplateHaskell #-}
module Application where
    
import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Heist

data App = App { 
        _heist :: Snaplet (Heist App)
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist
