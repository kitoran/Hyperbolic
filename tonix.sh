 #!/bin/bash

read os < currentos.txt
other='windows' 
if [ "$os" == "$other" ]
    then
        echo changingfromwindowstonix > currentos.txt
        mv .cabal-sandbox .cabal-sandbox-windows
        mv .cabal-sandbox-nix .cabal-sandbox
        mv cabal.config cabal.config-windows
        mv cabal.config-nix cabal.config
        mv dist dist-windows
        mv dist-nix dist
        mv cabal.sandbox.config cabal.sandbox.config-windows
        mv cabal.sandbox.config-nix cabal.sandbox.config
        echo nix > currentos.txt
    else
        echo "you'll have to do this manually. and fix currentos.txt"
fi
