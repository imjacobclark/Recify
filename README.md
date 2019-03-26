# Recify

Currently does the OAuth Dance to get a list of a users recently played Spotify tracks.

Eventually will recomend tracks to users based on their past listening history.

Access Tokens are stored in a single text file on disk after the initial authorization token is exchanged for an access token, this is far from ideal, but better than passing via query string, this should really go in a database - *this is only a proof of concept anyway*.

## Running

```shell
$ export bearer="Basic <CLIENT_ID:CLIENT_SECRET>"
$ export clientID="<CLIENT_ID>"
$ stack build && stack exec recify-exe
```

Then open localhost:3000/login in your web browser and login.