import Graphics.UI.WX hiding (Event)


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f       <- frame [text := "Counter"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    output  <- staticText f []
    
    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]

    let networkDescription :: MomentIO ()
        networkDescription = do
        
        eup   <- event0 bup   command
        edown <- event0 bdown command
        
        (counter :: Behavior Int)
            <- accumB 0 $ unions
                [ (+1)       <$ eup
                , subtract 1 <$ edown
                ]

        sink output [text :== show <$> counter] 

    network <- compile networkDescription    
    actuate network