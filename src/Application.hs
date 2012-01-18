{-# LANGUAGE TemplateHaskell #-}
module Application where
    
import Control.Concurrent (ThreadId)
import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Heist

data App = App { 
        _heist :: Snaplet (Heist App),
        _threadId :: ThreadId
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist
