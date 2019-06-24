# State transition logger for Miso [![Hackage](https://img.shields.io/hackage/v/miso-action-logger.svg)](http://hackage.haskell.org/package/miso-action-logger)

**MisoActionLogger** helps you quickly develop and debug your [Miso](https://github.com/dmjio/miso)
apps by wrapping your `update` function to log every evaluated action and show you the app state
before and after.

<img width="905" alt="Screenshot of the logger in operation" src="https://user-images.githubusercontent.com/895159/60028706-7dc14100-96a8-11e9-8bb8-691806d24228.png">

We marshal your state tree to JS Objects to benefit from the rich object introspection features
of modern browser consoles. This imposes a `ToJSON` constraint on your models.