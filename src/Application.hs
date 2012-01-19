{-# LANGUAGE TemplateHaskell #-}
module Application where
    
import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Heist

import Text.SyntaxHighlight.TextMate (TMSyntaxFile)

data App = App { 
        _heist :: Snaplet (Heist App),
        _cspmSynytaxFile :: TMSyntaxFile
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist
