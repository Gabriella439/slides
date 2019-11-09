% A bare-bones Twitter clone implemented with Haskell + Nix
% Gabriel Gonzalez
% April 18, 2017

# Overview

This talk illustrates how to implement and deploy a:

... bare-bones Twitter clone

... as a multi-page application

... using Haskell and Nix

The final implementation fits in a single file! (~450 lines of code)

# Overview

* **How web servers work**
* Using Nix to deploy a database and server skeleton to EC2
* Using Haskell to implement the business logic

# Single page vs multi page application

There are two extremes on the web application spectrum

* Multi-page applications (e.g. `dmv.ca.gov`)

  HTML is generated server-side.  The page only changes when the user clicks a
  link or submits a form:

  ```haskell
  { path : List Text, input : < GET : QueryParams | POST : FormParams > } → HTML
  ```

* Single-page applications (e.g. `gmail.com`)

  HTML is served only once for the initial page load, but after that JavaScript
  code communicates with the server on the user's behalf using JSON

  ```haskell
  { path : List Text, input : < GET : JSON | POST : JSON | … > } → JSON
  ```

Many web applications are somewhere in between (e.g. `github.com`)

# `yesod` vs. `servant`

`yesod` is primarily designed to support multi page web applications

`servant` is primarily designed to support single page web applications

They both can be used for other use cases, though!

For example, this talk will use `servant` to serve a multi page web application

This was the simplest to demo, not necessarily what I might use in production
