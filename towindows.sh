 #!/bin/bash

read os < currentos.txt
other='nix' 
if [ "$os" == "$other" ]
    then
        echo 'changingfromnixtowindows' > currentos.txt
        mv .cabal-sandbox .cabal-sandbox-nix
        mv .cabal-sandbox-windows .cabal-sandbox
        mv cabal.config cabal.config-nix
        mv cabal.config-windows cabal.config
        mv dist dist-nix
        mv dist-windows dist
        mv cabal.sandbox.config cabal.sandbox.config-nix
        mv cabal.sandbox.config-windows cabal.sandbox.config
        echo windows > currentos.txt
    else
        echo "you'll have to do this manually. and fix currentos.txt"
fi

