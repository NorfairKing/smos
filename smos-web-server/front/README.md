# smos-web-server-front

## Working on this

Run these commands in **two separate** terminals within a `nix-shell`, in order.

```shell
nix-shell --run "yarn watch"
```

```shell
nix-shell --run "yarn && yarn dev"
```

Open <http://localhost:1234> afterwards

## After adding a new dependency to the purescript code

```shell
spago2nix generate
```
