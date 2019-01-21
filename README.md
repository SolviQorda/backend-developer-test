# Braet - Connect with people, and then play board games with them.

An API where players of Chess, Settlers of Catan and Risk can find other players, and host games.

# Technical Choices

I had experience using Yesod to build Haskell webapps, and thought the most efficient route to deliver on the spec would be to use an environment I was familiar with. Yesod provides a web application framework for typesafe routes, and built-in libraries for integration with postgres via Persistent. In practice, converting it to an API came with some complications, principally around the authorisation layer.

In order to for players to see 'list of other players interested in the same game as them, sorted by location', I wanted to run that query as close to the data as possible. My reasoning was that there might be a high number of users, and the database might be remote to the app, and it would therefore be efficient to run the query in the database. I found that effective sorts based on longitude and latitude could be done with PostGis. I used Perstistent's rawSQL function to run the PostGis query passing the relevant info as Persistent parameters, rather than as strings, for protection against SQL injections.

In order to verify that the handlers were accessible by any frontend I modified the Purescript Halogen AJAX example to test the API. ([The relevant test is here](https://github.com/SolviQorda/backend-developer-test/blob/master/purescript-halogen-master/examples/effects-aff-ajax/src/Component.purs)). The first issues I had to resolve to make these tests work was to remove CSRF checking and adjust the CORS settings. As I had only used the cookie-based authentication provided by the Yesod Auth Google plugin, I decided at first to stick with cookie-based auth. I thought that I would mention switching to authorization header based authentication as possible improvement to make it easier for non browser clients to access the API. However, after I found that this wasn't redirecting back to the test client and therefore the cookie wasn't being sent back to the API, I decided to switch to the more useful approach rather than trying to make the less useful one work.

I tried adapting the YesodAuth instance but I found it hard to make progress with this. I spoke with my mentor on Thursday, and she advised removing the YesodAuth instance and writing a simple Handler to verify JWTs against Google's JWK and get the user's unique idenitifer. This also meant that the app no longer needed to hold onto a session and that now it was clients' responsibility to include the JWT in the authorization header of their requests.

In order to get the user's unique identifier directly from the JWT I adapted https://github.com/whittle/yesod-auth-jwt/blob/master/src/Yesod/Auth/JOSE/TokenValidator.hs#L37 to get the user's unique identifier (subject claim) from the JWT. This also allowed for a simpler data schema.

I'm using Docker for deployment as it allowed a straightforward interface for communicating between database and app, and there is a Postgres image preconfigured with Postgis. I am currently finalising the Docker configuration after which I'll deploy to Amazon EC2.

# Improvements

At the moment, the games are Text, so whoever writes the client is responsible for implementing the part of the spec that details that only certain games are supported. By representing games as the ids of records in a games table the API admin could control what games are supported and clients could retrive a list of supported games.

At the moment any number of users can request to join a host's game. The spec mentions player numbers for games so I'd like to add a way of accepting requests and determining whether a game was full or not.

I think it would be valuable to explore using Servant, Scotty or WAI on its own instead of Yesod.

Perhaps instead of EC2 Amazon might provide something specifically for Docker.

# Feedback on the challenge

I didn't have experience writing any kind of HTTP API before this challenge, so whilst I was tried to deliver on the spec I spent most of my time learning about HTTP, JSON and more API like authentication. It was quite late before I noticed some ambiguity in the spec regarding the meaning of hosting and requesting to join.
