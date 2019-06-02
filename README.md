# Recify

A proof of concept Spotify track recomender using K Nearest Neighbours.

## Tech 📸

- Spotify API (thanks Spotify)
- Haskell
- Scotty (Haskell Web Server)
- Aeson (Haskell JSON utilitiy)
- wreq (Haskell HTTP utility)

## Features 🚀

- [x] Authentication with Spotify via OAuth
- [x] Obtain a list of the most recent (latest 25) songs and basic metadata (Artist, Song Title, Explicit?)
- [x] Render those songs as HTML locally
- [x] Produce a CSV of songs (artist, song title, genre) locally on disk

## Coming soon 👨🏻‍💻

- [ ] Additional song metadata (Genre, Bars, Beats, Segments, Tatums, Tempo, Loudness, Mode, Pitches)
- [ ] Store most recent track listings (latest 25) in DynamoDB
- [ ] Recursivley gather all past listening history and store in  DynamoDB
- [ ] Listen and update DynamoDB when new tracks are played
- [ ] Populate DynamoDB with non-listened to song data to establish a our two distinct classes (listened too [liked] verses non-listened too [disliked])
- [ ] Perform a train a K Nearest Neighbours algorithm and to generate song recomendations from todays charts

## Warning 🚨

Whilst Recify only requests Access a scope of `user-read-recently-played, user-top-read` please be aware that tokens are stored in a single text file on disk after the initial authorization token is exchanged for an access token - *this will eventually be AES Encrypted and stored remotley*. `accessToken.txt` is `.gitignored`, do not share this file or its contents, it is advised you clean this file up when you're done - whilst the requested scope is non-destructive, better to be safe than sorry.

## Running 🔌

```shell
$ export bearer="Basic base64(<CLIENT_ID:CLIENT_SECRET>)"
$ export clientID="<CLIENT_ID>"
$ stack build && stack exec recify-exe
```

Then open [localhost:3000](localhost:3000) in your web browser and login.

## Technical Details

Currently Recify does the following:

1. Starts a web server on localhost:3000.
2. Shows a welcome screen at `localhost:3000/` with a hyperlink to begin the OAuth grant flow.
3. Upon initiating the grant flow `localhost:3000/grant` accepts a GET request and immediatley sets a HTTP Location header to the Spotify API authorize endpoint, providing a client secret, client ID, callback endpoint and an authorization scope of `user-read-recently-played, user-top-read`.
4. Upon being called back from Spotify the Authorization Token is lifted from the callbacks query string and a HTTP GET request is made back to the Spotify API to echange the Authorization Token for an Access Token which is subsequently written to disk and a 302 Redirect returned to the users browser redirecting them to the dashboard.
5. Upon landing on `localhost:3000/dashboard` the Access Token is loaded from disk and used to make a HTTP GET request to the Spotify API requesting recently played data.
6. This data is marshalled into an internal representation where it gets processed into HTML and returned as a 200 whilst also processed as CSV and written to disk.