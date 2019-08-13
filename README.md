# ForgeSwap

If a dApp wants to sell something to a customer on its application chain and wants something back as return on another chain (let's call it asset chain), then ForgeSwap service will be very helpful. You can think of this service as a cross-chain payment service that breaks the barriers between chains.

## How to run in Dev mode.

  1. Make sure you have latest [forge-patron](https://github.com/ArcBlock/forge-patron) installed.
  2. Make sure you have Postgres installed on your machine.
  3. `make dep`
  4. `make rebuild`
  5. `make build-db`
  6. `make run`
  
## Configuration

  1. The `FORGESWAP_CONFIG` environment variable specifies the path of the config file.
  2. The config file supports two form, *plain value* and *environment variable*.
      1. Plain value. For example: `name = "Application Name"`
      2. Environment variable: For example: `sk = "SYSTEM:ASSET_OWNER_SK"`. For this kind of config value, the ForgeSwap service will try to read the environment variable `ASSET_OWNER_SK` and use its value as `sk`.

## How to Use

### Create a swap as an application

As an application, the first thing to do is creating a swap. The most important information to tell the swap service are:
  * `userDid`: who is making the swap with dApp
  * `offerToken`, `offerAssets`: what is the dApp going to sell to the user.
  * `demandToken`, `demandAssets`: what does the dApp wants the user to pay.

Here is a quick example to create a swap. A dApp needs to make a HTTP call like this:

```http
POST http://host:port/api/swap

{
  "userDid": "z1VFy8hB9ndynkWAAH9P1a2L5WaU7AvtKGy",
  "offerToken": 1000000000000000000,
  "demandToken": 1000000000000000000
}
```

This API would return a response like: 
```json
{
  "response": {
    "id": "65ab7b5e-192c-48d4-bc4e-19c7d09cf6c3",
    "redirect": "http://localhost:8807/swap/65ab7b5e-192c-48d4-bc4e-19c7d09cf6c3"
  }
}
```

Besides the example displayed above, here is the full argument list supported by ForgeSwap Service:

* `userDid`: The address of the user the dApp wants to make swap with.
* `assetOwner`: The alias of asset owner in your config file.
* `offerAssets`: The list of asset addresses to sell to the user.
* `offerToken`: The token to sell to the user.
* `offerChain`: The alias of the chain to sell token and assets. The chain alias is what you put in the `chains` section in the config file.
* `offerLocktime`: The number of block to lock the swap.
* `demandAssets`: The list of asset addresses that the user is going to pay.
* `demandToken`: The token that the user is going to pay.
* `demandChain`: The alias of the chain where the user is going to pay. The chain alias is what you put in the `chains` section in the config file.
* `demandLocktime`: The number of block that users must lock their swap.

### Redirect the user's browser.

The response of the swap creation API contains two elements:
* `id`: The swap identification number which is UUID.
* `redirect`: The URL that the dApp should redirect the end user's browser to.

So the second thing, and also the last thing, to do is to redirect end user's browser to the URL. The target web page will guide the user to finish the swap by using the wallet. The entire swap process is handled by the service which means ForgeSwap service will set up and retrieve/revoke swap on the dApp's behalf. 
