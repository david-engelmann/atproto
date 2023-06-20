# atproto
OCaml-based AT Protocol tools

# ENV
You can create a .env file with the minimum two variables
- ATP_AUTH : EmailAddress:AppPassword
    Add your [App Password](https://bsky.app/settings/app-passwords)
    credentials (using your norma email address as the username)

- ATP_HOST : bsky.social
    Add your atp host url without schema

## Optional
- BASE_ENDPOINT : xrpc
    base endpoint, default xrpc

# Testing
testing can be ran from the root directory with the following command

```shell
$ dune runtest
```
