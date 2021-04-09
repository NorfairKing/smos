---
title: The Smos Server API
description: Documentation for the Smos API and how to call it
---


The Smos server API is currently undocumented, but it is defined using the very readable [Haskell's Servant Type-Level Web DSL](https://docs.servant.dev/en/stable/).
You can read it in the Smos repository at `smos-api/src/Smos/API.hs` and call it via the `smos-client` client library.


### API Versioning

The Smos server API offers the `/api-version` endpoint.
When implementing a client, it should first call the `/api-version` and check that the major version component of the response matches the major version component of the version the client was written against.
Otherwise the client will run into compatibility issues.

When using the `smos-client` Haskell client library, you can use the `clientWithVersionCheck` to have this taken care of automatically.
