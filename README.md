# haskell-workshop

Haskell Workshop - Spring 2016

## Overview

*See __HTTPS Fix__ below concerning breaking change in Reddit API.*

This is intended to be a zero-to-something introduction to Haskell for working programmers who are
interested in functional programming, but don't know where to start.

We assume a basic knowledge of web development in this tutorial. If you're familiar with basic
client/server interaction and HTML, this tutorial should be easy to follow!

The workshop will consist of two parts:

1. Matt Parsons will present an overview of the syntax/look and feel of Haskell, so you won't get lost! You can
follow along with this section using the `Syntax.hs` source file.
2. Benjamin Kovach will walk you through an implementation of a simple web application that reads posts from
multiple subreddit listings concurrently and presents them to a user in a web browser. We will work through this
together, fleshing out `MyReddit.hs` as we go.

## Getting Started

### Prerequisite tools you'll need

#### git

A version control system. You'll need this to pull down the boilerplate code located in this repository.

Installation instructions are located here, for various operating systems.

https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

#### stack

The haskell toolchain and package manager. Stack will install the Haskell compiler for you _and_ manage the dependencies of your project.

Installation instructions are located here, for various operating systems:

http://docs.haskellstack.org/en/stable/install_and_upgrade/#mac-os-x

#### a computer

You'll need this to compile and run your project.

#### an open mind

Haskell might look strange at first, but it's no more difficult than any other language! Keep an open mind and you'll be writing your own code in no time.

### Setting up the project

To get the project set up, you'll need to run the following series of commands:

```bash
$ git clone https://github.com/sellerlabs/haskell-workshop
$ cd haskell-workshop
$ stack setup
$ stack install
```

**This will take a long time the first time you do it!** If all goes well, you should see something along the following lines:

```
Copying from /Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin/redditui to /Users/Ben/.local/bin/redditui

Copied executables to /Users/Ben/.local/bin:
- redditui
```

If `/Users/$USER/.local/bin` is properly set in your `PATH`, you should be able to do this:

```hs
$ redditui
redditui: Not implemented...yet!
```

At this point, you should be good to go!

### HTTPS Fix

Accessing reddit using HTTP instead of HTTPS no longer works as shown during the live coding session. Here is the suggested fix to get `getReddit` working again.

#### `MyReddit.hs`

```hs
-- remove:
-- import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

-- add:
import qualified Network.Wreq as W (asJSON, get, responseBody)
import Control.Lens.Getter ((^.))
```

Replace `getReddit` with

```hs
getReddit :: String -> IO Listing
getReddit subreddit = do
  let url = "https://www.reddit.com/r/" <> subreddit <> "/hot/.json"
  r <- W.asJSON =<< W.get url
  pure (r ^. W.responseBody)
```

#### `haskell-workshop.cabal`

Add `containers`, `lens` and `wreq` to dependencies.

#### `stack`

`stack build` and—touch wood—you should be able to follow the tutorial.

### After the Workshop

Found yourself interested in learning more? Here's a short list of some great ways to start learning Haskell:

- [http://haskellbook.com/](http://haskellbook.com/) - An comprehensive, in-development book. Very up-to-date and extensive! Great for beginners and experts alike.
- [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - How Ben picked up the language. Showing a bit of age but a fun, engaging introduction nonetheless.
- [https://github.com/bitemyapp/learnhaskell](https://github.com/bitemyapp/learnhaskell) - Co-author of haskellbook's collection of resources on how to get started.
- [http://dev.stephendiehl.com/hask/](http://dev.stephendiehl.com/hask/) - Intermediate level collection of tips and tricks for getting more out of Haskell once you've got the basics down.
- [State of the Haskell Ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md) - A collection of topics, how well Haskell supports them, and what libraries are most useful for them
- [Haskell Is Easy](http://www.haskelliseasy.com) - A quick start guide to using Haskell
