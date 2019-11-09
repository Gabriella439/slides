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
* Deploy a blank server (NixOps)
* Add the database (Postgres)
* Add the web service (Haskell)
* Render the results (HTML + CSS)

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
  { path : List Text, input : < GET : JSON | POST : JSON | … > } → JSON
  ```

Many web applications are somewhere in between (e.g. `github.com`)

# Multi page application

![](./multi-page.png)

# Single page application

![](./single-page.png)

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
