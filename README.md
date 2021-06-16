# short-url
A Servant and Persitent backend that handles shortening of URLS.

The template [servant-persistent](https://github.com/parsonsmatt/servant-persistent) is used to get some nice logs and abstraction for testing.

This version has been updated to work on ghc 8.10.4 and all the libraries were updated too, which was... fun :sweat_smile: (view Painpoints for more details)


## Requirements:

#### Nix

Having nix installed will help with getting the right versions of ghc and the tooling.

Install nix -> `nix-shell` and et voila all built for you and ready to go.
(The caveat is installing nix might not be that straight forward)

### Haskell

You can use cabal to build the server:

2. `> cabal install --dependencies-only && cabal configure && cabal build`
3. `> cabal run shorturl`

### Database:

You will require 2 postgres database named:
`shorturl` + `shorturltest` <- this is used for the tests

you will also need a user `test` with the password wait for it `test`!
make sure to give access to both dbs.

## Developement

If you are using the nix the repo will install the correct version of the haskell language server.

alternatingly you can run the app inside GHCi by:

``` sh
> ghci
> :l DevelMain
> DevelMain.update
> DevelMain.shutdown
```
There is more information about this approach, on the wiki: [https://github.com/yesodweb/yesod/wiki/ghci](https://github.com/yesodweb/yesod/wiki/ghci)

## Test

Running the tests will require the second database a pre mentioned called `shorturltest`
Then to run the tests simply:
`> cabal run tests`

## Api

- GET `/${short}` will redirect with a 301, to attached original long url
- POST `/shorten` with JSON like `{ "url": "String"}` if succesfull it will return a 200 with some JSON `{"long": "String", short: "String" }`

## Using the API from the command line
```
# generate a short link
> curl --verbose --request POST --header "Content-Type: application/json" \
    --data '{"url": "custom url"}' \
	http://localhost:8081/shorten

# redirect a
> curl --verbose --request GET --header "Content-Type: application/json" \
	http://localhost:8081/${short}

```

## Visual representation

<p align="center"><img src="gif/shorturl.gif" height="600px" /></p>


## Painpoints

Upgrading the project wasn't as straight forward as expected, there was a lot of blockers with clashing libraries and missing functions etc.

Creating a custom `Verb` for doing the redirection.

Remembering/learning how `Servant` + `Persistent` worked and changing all that to fit it to my requiements

Getting the tests to check things properlly, though they are not fully done as the quickcheck/unit tests don't do anything exiting!
