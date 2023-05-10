import Menu

main :: IO ()
main = do
    selection <-
        getMenuSelection
            [ PathOption "Project 1" "./path/to/1"
            , PathOption "Project 2" "./path/to/2"
            , PathOption "Project 3" "./path/to/3"
            ]
    print selection
