## MisoActionLogger [![Hackage](https://img.shields.io/hackage/v/miso-action-logger.svg)](http://hackage.haskell.org/package/miso-action-logger)
### State transition logger for Miso 

**MisoActionLogger** helps you quickly develop and debug your [Miso](https://github.com/dmjio/miso)
apps by wrapping your `update` function to log every evaluated action and show you the app state
before and after.

<img width="905" alt="Screenshot of the logger in operation" src="https://user-images.githubusercontent.com/895159/60028706-7dc14100-96a8-11e9-8bb8-691806d24228.png">

We marshal your state tree to JS Objects to benefit from the rich object introspection features
of modern browser consoles. This imposes a `ToJSON` constraint on your models.

### Installation

**Option 1:** use `nix-prefetch-git` to generate arguments for `fetchFromGitHub`.
You'll end up with somthing like this:
```nix
miso-action-logger = ghcjs.callCabal2nix "miso-action-logger" (pkgs.fetchFromGitHub {
  owner = "Lermex";
  repo = "miso-action-logger";
  sha256 = "1q6bpckz355paxcs10223fnl1d3lnxz2vcgj8l7nqvclicp04hsb";
  rev = "917af7edc33e86b1510ebf2bc8e1b1bb9f03c164";
}) { miso = miso-ghcjs; }
```  

**Option 2:** if your nixpkgs is recent enough, you can use `pkgs.callHackageDirect` to fetch
directly from Hackage.

In both cases you will then need to add the package to your build arguments like this:
```nix
ghcjs.callCabal2nix "client" ./. {
  miso = miso-ghcjs;
  miso-action-logger = miso-action-logger;
}
```

### Usage

```haskell
module Main where

import MisoActionLogger

-- ...

main :: IO ()
main = do
  let model = Model -- ...
  startApp App {model = model, ..}
  where
    initialAction = NoOp 
    update = defaultActionLogger updateModel -- <-- wrap your update function 
    view   = Routing.view            
    events = defaultEvents        
    subs   = [ uriSub HandleURI ]                   
    mountPoint = Nothing   
```