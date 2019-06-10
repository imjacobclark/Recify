# Recify

A proof of concept Spotify track recomender using K Nearest Neighbours.

This is pretty rough around the edges, I haven't unit tested it, rolled my own simple Mustache and CSV parser and generally thrown caution to the wind **in order to focus on the core purpose of the project** (which is learning Haskell, KNN and the Spotify API), I'm still learning Haskell, so a lot of this won't be best practice.

PRs welcome, feedback welcome - once "complete" I'll probably write up my approach, see _Technical Details_ below for more on that now without having to go through the source code or wait 'til it's complete.

## Tech 📸

- Spotify API (thanks Spotify)
- Haskell via Stack
- Scotty (Haskell Web Server)
- Aeson (Haskell JSON utilitiy)
- wreq (Haskell HTTP utility)

## Features 🚀

- [x] Authentication with Spotify via OAuth 2.0
- [x] Obtain a list of the most recent (latest 25) songs and basic metadata (Artist, Song Title, Explicit?)
- [x] Render those songs as HTML locally
- [x] Produce a CSV of songs (artist, song title, genre) locally on disk

## Coming soon 👨🏻‍💻

- [ ] Additional song metadata (Genre, Bars, Beats, Segments, Tatums, Tempo, Loudness, Mode, Pitches)
- [ ] Store most recent track listings (latest 25) in DynamoDB
- [ ] Recursivley gather all past listening history and store in  DynamoDB
- [ ] Listen and update DynamoDB when new tracks are played
- [ ] Populate DynamoDB with non-listened to song data to establish our two distinct classes (listened too [liked] verses non-listened too [disliked])
- [ ] Perform a train a K Nearest Neighbours algorithm and to generate song recomendations from todays charts
- [ ] Add an authentication provider to protect delegated access tokens

## Warning 🚨

There is no authentication implemented in Recify and access tokens are stored in plaintext as `HttpOnly` cookies. 

Recify provides no guarantees that the person accessing Recify is the Resource Owner. Authroization to access a users profile data is simplfy delegated to Recify from Spotify by approval of a Resource Owner at some point in time, there are no guarantees they are still present once the Access Token has been minted. Recify only requests a scope of `user-read-recently-played, user-top-read`, which is a non-destructive, read only grant and discloses no personal information about the user.

`HttpOnly` cookies means that the cookie cannot be lifted by arbitary JavaScript on the page, so is immune to XSS attack (it is only present during the HTTP request, response lifecycle). However it would not stop a third party actor physically accessing the device and lifting the cookie from the users browser.

Adding proper Authentication to Recify is a "Coming Soon" feature, this would allow a user to identify with a third party service. With this in place Recify could validate that a user is who they say they are and once satisfied, allow access to the Spotify OAuth 2.0 Access Token Recify will be storing and refreshing on behalf of the user.

## Running 🔌

### Locally

First, [ensure you have Stack installed](https://docs.haskellstack.org/en/stable/README/), this project uses it to compile and execute Haskell code.

You'll need a [Spotify developer account](https://developer.spotify.com/dashboard/applications) where you'll create a new application in order to obtain you applications *Client ID* and *Client Secret* - the Client Secret should be treated as you would a normal password. These two values together with some processing produce your bearer token which identifies you to the Spotify API and allow you to configure callbacks, they also ensure clients don't abuse the Spotify API.

In order to process a bearer token, you need to obtain both the client ID and secret from your application within the Spotify API dashboard, you'll then concatinate them together with a delimiter of `:` and base64 encode them. Heres a little JavaScript snippet you can run in a browser console to do just that:

```javascript
btoa("replace_with_you_Client_ID" + ":" + "replace_with_you_Client_Secret")
```

Once you have your base64 encoded credentials, we can set them as environment variables on your local machine (note: we do this to protect the credentials, environment variables ensure they are not checked into version control or stored on disk, although they are still visible to anybody with access to the machine).

```shell
$ export bearer="Basic replace_with_your_bearer_token"
```

Spotify also needs Client ID sent in plain text (this is OK as its not the secret), so we create an environment variable for that too.

```shell
$ export clientID="replace_with_your_client_id"
```

Lets grab our dependencies, compile and start Recify. The `local-dev` script starts Recify on port 3000 via localhost.

```shell
$ bash scripts/local-dev.sh
```

Once this successfully completes, you can open [localhost:3000](localhost:3000) in your web browser and follow the instructions.

### Heroku

Create your Heroku project [as you normally would](https://devcenter.heroku.com/categories/command-line) and attach the Stack buildpack.

```shell
$ heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack replace_with_your_heroku_app_name
```

You'll need to set the `bearer`, `clientID` and `fqdn` environment variables within your Heroku app, once set you can deploy as normal, the Procfile already exists within this repo.

## Technical Details ✍️

Currently Recify does the following:

1. Starts a web server on localhost:3000.
2. Shows a welcome screen at `localhost:3000/` with a hyperlink to begin the OAuth grant flow.
3. Upon initiating the grant flow `localhost:3000/grant` accepts a GET request and immediatley sets a HTTP Location header to the Spotify API authorize endpoint, providing a client secret, client ID, callback endpoint and an authorization scope of `user-read-recently-played, user-top-read`.
4. Upon being called back from Spotify the Authorization Token is lifted from the callbacks query string and a HTTP GET request is made back to the Spotify API to echange the Authorization Token for an Access Token which is subsequently written to disk and a 302 Redirect returned to the users browser redirecting them to the dashboard.
5. Upon landing on `localhost:3000/dashboard` the Access Token is loaded from disk and used to make a HTTP GET request to the Spotify API requesting recently played data.
6. This data is marshalled into an internal representation where it gets processed into HTML and returned as a 200 whilst also processed as CSV and written to disk.