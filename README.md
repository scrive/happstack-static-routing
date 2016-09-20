# happstack-static-routing [![Hackage version](https://img.shields.io/hackage/v/happstack-static-routing.svg?label=Hackage)](https://hackage.haskell.org/package/happstack-static-routing) [![Build Status](https://secure.travis-ci.org/scrive/happstack-static-routing.svg?branch=master)](http://travis-ci.org/scrive/happstack-static-routing)

If you have a large routing table in Happstack and want to insert a
new handler, you might run into overlap problems (ambiguity).  The new
handler might not fire because it matches against a URL that is
already handled earlier.  Or if you put your new handler first, it
might steal requests from existing handlers.

This Happstack support library allows you to detect overlap cases and
build unambiguous routing tables where the order of the handlers is
irrelevant.
