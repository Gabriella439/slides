% A bare-bones Twitter clone implemented with Haskell + Nix
% Gabriella Gonzalez
% June 13, 2020

# Simplified explanation of Twitter

Twitter is a social media site where users can tweet and follow other users.

The original Twitter timeline showed tweets from people you followed in reverse
chronological order.

![](./twitter.png)

# Overview

This talk illustrates how to implement and deploy a:

... bare-bones Twitter clone

... as a multi-page application

... using Haskell and Nix

The final implementation fits in a single file! (~470 lines of code)

* ~340 lines of Haskell code
* ~100 lines of Nix
* ~30 lines of SQL

You can find the final result of this presentation at:

* [https://github.com/Gabriella439/simple-twitter](https://github.com/Gabriella439/simple-twitter)

# Overview

* **How web servers work**
* Deploy a blank server (NixOps)
* Add the database (Postgres)
* Add the web service (Haskell)
* Render the results (HTML + CSS)
* Conclusion

# Single page vs multi page application

There are two extremes on the web application spectrum:

* Multi-page applications (e.g. `dmv.ca.gov`)

  HTML is generated server-side.  The page only changes when the user clicks a
  link or submits a form:

  ```haskell
  { path : List Text, input : < GET : QueryParams | POST : FormParams > } → HTML
  ```

* Single-page applications (e.g. `gmail.com`)

  HTML is served only once for the initial page load, but after that JavaScript
  code communicates with the server on the user's behalf using (usually) JSON

  ```haskell
  { path : List Text, input : < GET : QueryParams | POST : JSON | … > } → JSON
  ```

Many web applications are somewhere in between (e.g. `github.com`)

# `yesod` vs. `servant`

**`yesod`** is primarily designed to support **multi page web applications**

**`servant`** is primarily designed to support **single page web applications**

They both can be used for other use cases, though!

For example, this talk uses **`servant`** for a **multi page web application**

This is simple to demo, but not necessarily the approach I'd use in production

# Anatomy of our multi-page application

* Front-end: HTML + CSS
* Web service: ⚡️ Haskell
* Database: Postgres

![](./anatomy.png)

# Overview

* How web servers work
* **Deploy a blank server (NixOps)**
* Add the database (Postgres)
* Add the web service (Haskell)
* Render the results (HTML + CSS)
* Conclusion

# A blank EC2 server

```nix
let
  region = "us-west-1";

  accessKeyId = "default";

in
  { machine = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit accessKeyId region;

          instanceType = "t2.nano";

          keyPair = resources.ec2KeyPairs.my-key-pair;
        };
      };
    };

    resources = {
      ec2KeyPairs.my-key-pair = { inherit region accessKeyId; };

      ec2SecurityGroups = {
        "ssh" = {
          inherit accessKeyId region;

          rules = [
            { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
          ];
        };
      };
    };
  }
```

# Deploying the server

```bash
$ nixops create --deployment simple-twitter simple-twitter.nix

$ nixops deploy --deployment simple-twitter
…
machine....> activation finished successfully
simple-twitter> deployment finished successfully

$ nixops ssh --deployment simple-twitter machine

[root@machine:~]# systemctl status sshd
● sshd.service - SSH Daemon
   Loaded: loaded (/nix/store/d8a8ak0xsbzqq54b4wfgp98flhvkyfjx-unit-sshd.service/sshd.service; enabled; vendor preset: enabled)
   Active: active (running) since Sat 2019-11-09 16:16:28 UTC; 17min ago
 Main PID: 1710 (sshd)
       IP: 46.0K in, 111.9K out
    Tasks: 1
   Memory: 4.0M
      CPU: 43ms
   CGroup: /system.slice/sshd.service
           └─1710 /nix/store/hj338597zkps02cai2jcywxwq2iihjdi-openssh-7.9p1/bin/sshd -f /etc/ssh/sshd_config

Nov 09 16:27:19 machine sshd[1919]: Starting session: command for root from 69.181.64.30 port 57957 id 0
Nov 09 16:27:19 machine sshd[1919]: Close session: user root from 69.181.64.30 port 57957 id 0
Nov 09 16:31:44 machine sshd[2087]: Connection from 69.181.64.30 port 57997 on 172.31.2.114 port 22
Nov 09 16:31:44 machine sshd[2087]: Failed publickey for root from 69.181.64.30 port 57997 ssh2: RSA SHA256:ZQqcv3dtDCKTtoKPIbk5mFBoXjFzCjhhNIsnkdmBXLE
Nov 09 16:31:44 machine sshd[2087]: Accepted key RSA SHA256:ShyPdJfY4vOyJ8AdEhoXtkNICVUn88BqLvce9TfMZas found at /root/.ssh/authorized_keys:1
Nov 09 16:31:44 machine sshd[2087]: Postponed publickey for root from 69.181.64.30 port 57997 ssh2 [preauth]
Nov 09 16:31:44 machine sshd[2087]: Accepted key RSA SHA256:ShyPdJfY4vOyJ8AdEhoXtkNICVUn88BqLvce9TfMZas found at /root/.ssh/authorized_keys:1
Nov 09 16:31:44 machine sshd[2087]: Accepted publickey for root from 69.181.64.30 port 57997 ssh2: RSA SHA256:ShyPdJfY4vOyJ8AdEhoXtkNICVUn88BqLvce9TfMZas
Nov 09 16:31:44 machine sshd[2087]: pam_unix(sshd:session): session opened for user root by (uid=0)
Nov 09 16:31:44 machine sshd[2087]: Starting session: shell on pts/0 for root from 69.181.64.30 port 57997 id 0
```

# Configuring options

```nix
…
  { machine = { resources, ... }: {
      …

      networking.firewall.allowedTCPPorts = [ 80 ];  # ← New NixOS option
    };

    …
  }
```

# Applying the option change

```bash
$ nixops deploy --deployment simple-twitter
…
machine....> reloading the following units: firewall.service
machine....> activation finished successfully
simple-twitter> deployment finished successfully

$ nixops ssh --deployment simple-twitter machine

[root@machine:~]# iptables --list nixos-fw --numeric
Chain nixos-fw (1 references)
target     prot opt source               destination         
nixos-fw-accept  all  --  0.0.0.0/0            0.0.0.0/0           
nixos-fw-accept  all  --  0.0.0.0/0            0.0.0.0/0            ctstate RELATED,ESTABLISHED
nixos-fw-accept  tcp  --  0.0.0.0/0            0.0.0.0/0            tcp dpt:22
nixos-fw-accept  tcp  --  0.0.0.0/0            0.0.0.0/0            tcp dpt:80
nixos-fw-accept  icmp --  0.0.0.0/0            0.0.0.0/0            icmptype 8
nixos-fw-log-refuse  all  --  0.0.0.0/0            0.0.0.0/0           
```

# Browse NixOS options

You can browse available NixOS options by visiting:

* [https://nixos.org/nixos/options.html](https://nixos.org/nixos/options.html)

![](./nixos-options.png)

# Overview

* How web servers work
* Deploy a blank server (NixOps)
* **Add the database (Postgres)**
* Add the web service (Haskell)
* Render the results (HTML + CSS)
* Conclusion

# Schema

```sql
CREATE TABLE "user" (
  name text NOT NULL,
  PRIMARY KEY (name)
);

CREATE TABLE tweet (
  id integer GENERATED ALWAYS AS IDENTITY,
  contents text NOT NULL,
  time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id)
);

CREATE TABLE user_tweet (
  "user" text NOT NULL,
  tweet integer NOT NULL,
  PRIMARY KEY ("user", tweet),
  FOREIGN KEY ("user") REFERENCES "user" (name) ON DELETE CASCADE,
  FOREIGN KEY (tweet) REFERENCES tweet (id) ON DELETE CASCADE
);

CREATE TABLE follows (
  follower text NOT NULL,
  followed text NOT NULL,
  PRIMARY KEY (follower, followed),
  FOREIGN KEY (follower) REFERENCES "user" (name) ON DELETE CASCADE,
  FOREIGN KEY (followed) REFERENCES "user" (name) ON DELETE CASCADE
);
```

# Creating the database

```nix
…
  { machine = { pkgs, resources, ... }: {
      …

      services.postgresql = {
        enable = true;

        authentication = ''
          local all all ident map=mapping
        '';

        identMap = ''
          mapping root     postgres
          mapping postgres postgres
        '';

        package = pkgs.postgresql_11;

        initialScript = pkgs.writeText "initialScript.sql" ''
          CREATE TABLE "user" (
            name text NOT NULL,
            PRIMARY KEY (name)
          );

          …
        '';
      };
    };

    …
  }
```

# Deploying the database

```bash
$ nixops deploy --deployment simple-twitter
…
machine....> the following new units were started: postgresql.service
machine....> activation finished successfully
simple-twitter> deployment finished successfully

$ nixops ssh --deployment simple-twitter machine

[root@machine:~]# sudo --user postgres psql
psql (11.5)
Type "help" for help.

postgres=# \d "user"
              Table "public.user"
 Column | Type | Collation | Nullable | Default 
--------+------+-----------+----------+---------
 name   | text |           | not null | 
Indexes:
    "user_pkey" PRIMARY KEY, btree (name)
Referenced by:
    TABLE "follows" CONSTRAINT "follows_followed_fkey" FOREIGN KEY (followed) REFERENCES "user"(name) ON DELETE CASCADE
    TABLE "follows" CONSTRAINT "follows_follower_fkey" FOREIGN KEY (follower) REFERENCES "user"(name) ON DELETE CASCADE
    TABLE "user_tweet" CONSTRAINT "user_tweet_user_fkey" FOREIGN KEY ("user") REFERENCES "user"(name) ON DELETE CASCADE
```

# Overview

* How web servers work
* Deploy a blank server (NixOps)
* Add the database (Postgres)
* **Add the web service (Haskell)**
* Render the results (HTML + CSS)
* Conclusion

# A minimal Haskell service

```haskell
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Options.Generic (ParseRecord)
import Servant.API (Get, JSON)

import qualified Control.Exception          as Exception
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Options.Generic            as Options
import qualified Servant.Server             as Server

newtype Options = Options { connectPort :: Word16 }
    deriving stock (Generic)
    deriving anyclass (ParseRecord)

type API = Get '[JSON] Text

main :: IO ()
main = do
    Options{..} <- Options.getRecord "Simple Twitter"

    let connectInfo =
            PostgreSQL.defaultConnectInfo
              { PostgreSQL.connectPort = connectPort
              , PostgreSQL.connectHost = ""
              }

    let open = PostgreSQL.connect connectInfo
    let close = PostgreSQL.close

    Exception.bracket open close \_connection -> do
        let server = return "Hello, world!"

        let application = Server.serve @API Proxy server

        Warp.run 80 application
```

# Creating a skeleton for our service

```nix
…
  { machine = { config, pkgs, resources, ... }: {
      systemd.services.simple-twitter = {
        wantedBy = [ "multi-user.target" ];

        after = [ "postgresql.service" ];

        script =
          let
            ghc =
              pkgs.haskellPackages.ghcWithPackages (pkgs: [
                  pkgs.optparse-generic
                  pkgs.postgresql-simple
                  pkgs.servant
                  pkgs.servant-server
                  pkgs.warp
                ]
              );

            code = pkgs.writeText "Main.hs" ''
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
…
        Warp.run 80 application
            '';

            simple-twitter = pkgs.runCommand "simple-twitter" {} ''
              ${pkgs.coreutils}/bin/mkdir --parents $out/bin

              ${ghc}/bin/ghc -O -Wall -Werror ${code} -o $out/bin/simple-twitter
            '';

          in
            ''
            ${simple-twitter}/bin/simple-twitter --connectPort ${toString config.services.postgresql.port}
            '';
      };
    };

    resources = {
      …

      ec2SecurityGroups = {
        "http" = {
          inherit accessKeyId region;

          rules = [
            { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
          ];
        };

        …
      };
    };

    deployment = {
      …

      ec2 = {
        …

        securityGroups = [
          resources.ec2SecurityGroups."http"
          resources.ec2SecurityGroups."ssh"
        ];
      };
    };
  }
```

# Deploy our service

```
$ nixops deploy --deployment simple-twitter
…
machine....> starting the following units: simple-twitter.service
machine....> activation finished successfully
simple-twitter> deployment finished successfully

$ nixops info --deployment simple-twitter
Network name: simple-twitter
…

+-------------+-----------------+--------------------------------+-------------+--------------+
| Name        |      Status     | Type                           | Resource Id | IP address   |
+-------------+-----------------+--------------------------------+-------------+--------------+
| machine     | Up / Up-to-date | ec2 [us-west-1c; t2.nano]      | …           | 54.67.99.168 |
| my-key-pair |        Up       | ec2-keypair [us-west-1]        | …           |              |
| http        |        Up       | ec2-security-group [us-west-1] | …           |              |
+-------------+-----------------+--------------------------------+-------------+--------------+

$ curl http://54.67.99.168
"Hello, world!"
```

Not covered in this talk: how to register a domain name and enable HTTPS

# Define our API

The endpoints we will use to power our Twitter-like site are:

* `GET /` - Get the global timeline

* `POST /user` (†) - Create a user

* `GET /user` - Get a specific user's profile (tweets, timeline, follows)

* `POST /user/delete` (‡) - Delete a user

* `GET /users` - Get a list of all users

* `POST /tweet` (†) - Create a tweet

* `POST /follow` (†) - Follow a user

†: These methods should really be `PUT`

‡: This method should really be `DELETE /user`

Browser links and web forms only support `GET`/`POST`

# Our `Servant` API

```haskell
newtype User = User { name :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, FromRow, ToRow)
    deriving newtype (FromHttpApiData)

data Follow = Follow { follower :: Text, followed :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, ToRow)

data Tweet = Tweet { name :: Text, contents :: Text }
    deriving stock (Generic)
    deriving anyclass (FromForm, FromRow)

type API =
        -- GET /
        Get '[HTML] Markup
        -- POST /user
   :<|> "user" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup
        -- GET /user?name=:name
   :<|> "user" :> QueryParam' '[Required, Strict] "name" User :> Get '[HTML] Markup
        -- POST /user/delete
   :<|> "user" :> "delete" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup
        -- GET /users
   :<|> "users" :> Get '[HTML] Markup
        -- POST /tweet
   :<|> "tweet" :> ReqBody '[FormUrlEncoded] Tweet :> Post '[HTML] Markup
        -- POST /follow
   :<|> "follow" :> ReqBody '[FormUrlEncoded] Follow :> Post '[HTML] Markup
```

# Handler types

```haskell
>>> import Data.Text as Text
>>> import GHC.Generics as Generics
>>> import Servant
>>> import Servant.HTML.Blaze as Servant.Blaze
>>> import Text.Blaze.Internal as Blaze
>>> import Web.FormUrlEncoded as Web
>>> :set -XDeriveGeneric -XDeriveAnyClass -XGeneralizedNewtypeDeriving -XDerivingStrategies
>>> :set -XDataKinds -XTypeOperators
>>> newtype User = User { name :: Text } deriving stock (Generic) deriving anyclass (FromForm) deriving newtype (FromHttpApiData)

>>> :kind! Server (Get '[HTML] Markup)
= Handler (MarkupM ())

>>> :kind! Server ("user" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup)
= User -> Handler (MarkupM ())

>>> :kind! Server ("user" :> QueryParam' '[Required, Strict] "name" User :> Get '[HTML] Markup)
= User -> Handler (MarkupM ())
```

# Handlers

```haskell
let index :: Handler Markup
    index = do
        tweets <- query_ [sql|
            SELECT "user".name, tweet.contents
            FROM           "user"
                INNER JOIN user_tweet ON "user".name = user_tweet."user"
                INNER JOIN tweet      ON user_tweet.tweet = tweet.id
            ORDER BY tweet.time DESC
        |]

        …  -- Render the global timeline

let getUsers :: Handler Markup
    getUsers = do
        users <- query_ [sql|SELECT name FROM "user"|]

        …  -- Render the list of users

let createUser :: User -> Handler Markup
    createUser user = do
        execute user [sql|INSERT INTO "user" (name) VALUES (?)|]

        getUsers

let getUser :: User -> Handler Markup
    getUser user = do
        followeds <- query user [sql|
            SELECT follows.followed
            FROM           "user"
                INNER JOIN follows ON "user".name = follows.follower
            WHERE "user".name = ?
        |]

        history <- query user [sql|
            SELECT "user".name, tweet.contents
            FROM           "user"
                INNER JOIN user_tweet ON "user".name = user_tweet."user"
                INNER JOIN tweet      ON user_tweet.tweet = tweet.id
            WHERE "user".name = ?
            ORDER BY tweet.time DESC
        |]

        timeline <- query user [sql|
            SELECT follows.followed, tweet.contents
            FROM           "user"
                INNER JOIN follows    ON "user".name = follows.follower
                INNER JOIN user_tweet ON follows.followed = user_tweet."user"
                INNER JOIN tweet      ON user_tweet.tweet = tweet.id
            WHERE "user".name = ?
            ORDER BY tweet.time DESC
        |]

        …  -- Render the user's profile and timeline

let deleteUser :: User -> Handler Markup
    deleteUser user = do
        execute user [sql|DELETE FROM "user" WHERE name = ?|]

        getUsers

let createTweet :: Tweet -> Handler Markup
    createTweet (Tweet {..}) = do
        rows <- query (Only contents) [sql|
            INSERT INTO tweet (contents) VALUES (?) RETURNING (id)
        |]

        id <- case rows of
            [ (id :: Only Integer) ] -> return id
            _                        -> Catch.throwM Server.err500

        execute (Only name :. id) [sql|
            INSERT INTO user_tweet ("user", tweet) VALUES (?, ?)
        |]

        getUser (User {..})

let follow :: Follow -> Handler Markup
    follow f = do
        execute f [sql|
            INSERT INTO follows (follower, followed) VALUES (?, ?)
        |]

        getUser (User { name = follower })

let server = index
        :<|> createUser
        :<|> getUser
        :<|> deleteUser
        :<|> getUsers
        :<|> createTweet
        :<|> follow
```

# Overview

* How web servers work
* Deploy a blank server (NixOps)
* Add the database (Postgres)
* Add the web service (Haskell)
* **Render the results (HTML + CSS)**
* Conclusion

# Rendering HTML

We'll use the `blaze-markup` package to render HTML using Haskell code:

```haskell
-- <button type="submit" class="btn btn-primary btn-sm">${label}</button>
let submit label =
            Html.button
        !   Attr.type_ "submit"
        !   Attr.class_ "btn btn-primary btn-sm"
        $   label

{-  <div class="form-group">
      <input type="text" class="form-control form-control-sm" name="${name}" placeholder="${name}">
    </div>
-}
let field name = do
        Html.div ! Attr.class_ "form-group" $ do
            Html.input
                !   Attr.type_ "text"
                !   Attr.class_ "form-control form-control-sm"
                !   Attr.name name
                !   Attr.placeholder name

{-  <div class="col-md-4">
        <form action="${action}" method="${method}" class="border m-3 p-2 bg-light">
        ${html}
        </form>
    </div>
-}
let form action method html =
        Html.div ! Attr.class_ "col-md-4" $ do
            Html.form
                !   Attr.action action
                !   Attr.method method
                !   Attr.class_ "border m-3 p-2 bg-light"
                $   html

let forms = do
        Html.div ! Attr.class_ "row" $ do
            form "/" "get" do
                submit "Global timeline"

        Html.div ! Attr.class_ "row" $ do
            form "/user" "post" do
                field "name"
                submit "Create user"

            form "/user/delete" "post" do
                field "name"
                submit "Delete user"

            form "/users" "get" do
                submit "Get users"

        Html.div ! Attr.class_ "row" $ do
            form "/tweet" "post" do
                field "name"
                field "contents"
                submit "Create tweet"

            form "/follow" "post" do
                field "follower"
                field "followed"
                submit "Follow"

            form "/user" "get" do
                field "name"
                submit "Get user"

let ul html = Html.ul ! Attr.class_ "list-group" $ html

let li html = Html.li ! Attr.class_ "list-group-item" $ html

let wrap body =
        Html.html do
            Html.head do
                Html.title "Simple Twitter"
                Html.link
                    ! Attr.rel "stylesheet"
                    ! Attr.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

            Html.body do
                Html.h1
                    ! Attr.class_ "display-4 text-center"
                    $ "Simple Twitter"

                Html.div ! Attr.class_ "container" $ do
                  Html.div ! Attr.class_ "row" $ do
                      Html.div ! Attr.class_ "col-md-6" $ forms
                      Html.div ! Attr.class_ "col-md-6" $ body

let failWith message handler = do
        let fallback :: SomeException -> Handler Markup
            fallback _ = return (wrap (Html.toHtml message))

        Catch.handle fallback handler

let index :: Handler Markup
    index = do
        tweets <- query_ [sql|
            SELECT "user".name, tweet.contents
            FROM           "user"
                INNER JOIN user_tweet ON "user".name = user_tweet."user"
                INNER JOIN tweet      ON user_tweet.tweet = tweet.id
            ORDER BY tweet.time DESC
        |]

        let renderTweet (Tweet {..}) =
                li (Html.toHtml (name <> ": " <> contents))

        return do
            wrap (ul (traverse_ renderTweet tweets))

let getUsers :: Handler Markup
    getUsers = do
        users <- query_ [sql|SELECT name FROM "user"|]

        let renderUser (User {..}) = li (Html.toHtml name)

        return (wrap (ul (traverse_ renderUser users)))

let createUser :: User -> Handler Markup
    createUser user@User{..} = do
        let message =
                "A user named '" <> name <> "' already exists"

        failWith message do
            execute user [sql|INSERT INTO "user" (name) VALUES (?)|]

            getUsers

let getUser :: User -> Handler Markup
    getUser user@User{..} = do
        let message =
                "No user named '" <> name <> "' exists"

        failWith message do
            followeds <- query user [sql|
                SELECT follows.followed
                FROM           "user"
                    INNER JOIN follows ON "user".name = follows.follower
                WHERE "user".name = ?
            |]

            history <- query user [sql|
                SELECT "user".name, tweet.contents
                FROM           "user"
                    INNER JOIN user_tweet ON "user".name = user_tweet."user"
                    INNER JOIN tweet      ON user_tweet.tweet = tweet.id
                WHERE "user".name = ?
                ORDER BY tweet.time DESC
            |]

            timeline <- query user [sql|
                SELECT follows.followed, tweet.contents
                FROM           "user"
                    INNER JOIN follows    ON "user".name = follows.follower
                    INNER JOIN user_tweet ON follows.followed = user_tweet."user"
                    INNER JOIN tweet      ON user_tweet.tweet = tweet.id
                WHERE "user".name = ?
                ORDER BY tweet.time DESC
            |]

            let renderHistory (Tweet { contents }) =
                    li (Html.toHtml contents)

            let renderTimeline (Tweet { name = followed, ..}) =
                    li (Html.toHtml (followed <> ": " <> contents))

            let renderUser (User { name = followed }) =
                    li (Html.toHtml followed)

            return do
                wrap do
                    Html.h2 (Html.toHtml name)
                    Html.hr
                    Monad.when (not (null history)) do
                        Html.h3 "History"
                        ul (traverse_ renderHistory history)

                    Monad.when (not (null timeline)) do
                        Html.h3 "Timeline"
                        ul (traverse_ renderTimeline timeline)

                    Monad.when (not (null followeds)) do
                        Html.h3 "Follows"
                        ul (traverse_ renderUser followeds)

let deleteUser :: User -> Handler Markup
    deleteUser user@User{..}= do
        let message =
                "No user named '" <> name <> "' exists"

        failWith message do
            execute user [sql|DELETE FROM "user" WHERE name = ?|]

            getUsers

let createTweet :: Tweet -> Handler Markup
    createTweet (Tweet {..}) = do
        rows <- query (Only contents) [sql|
            INSERT INTO tweet (contents) VALUES (?) RETURNING (id)
        |]

        id <- case rows of
            [ (id :: Only Integer) ] -> return id
            _                        -> Catch.throwM Server.err500

        execute (Only name :. id) [sql|
            INSERT INTO user_tweet ("user", tweet) VALUES (?, ?)
        |]

        getUser (User {..})

let follow :: Follow -> Handler Markup
    follow f@Follow{..} = do
        let message =
                "'" <> follower <> "' already follows '" <> followed <> "'"
        failWith message do
            execute f [sql|
                INSERT INTO follows (follower, followed) VALUES (?, ?)
            |]

            getUser (User { name = follower })
```

# Overview

* How web servers work
* Deploy a blank server (NixOps)
* Add the database (Postgres)
* Add the web service (Haskell)
* Render the results (HTML + CSS)
* **Conclusion**

# Conclusion

Play with the Simple Twitter site I deployed!

Haskell + Nix + Postgres can be used to build real web applications

You can find the complete code here:

* [https://github.com/Gabriella439/simple-twitter](https://github.com/Gabriella439/simple-twitter)
