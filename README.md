# ForgeSwap

## How to run with multi-chain topology.

  1. Clean your current ~/.forge_patron dir.
  2. `forge-patron init`
  3. Update your ~/.forge_patron/topology.yml with following content
     ```yml
      ---
      type: topology
      chains:
        - chain_id: asset_chain
          validators:
            - '[address: "127.0.0.1", p2p: 10000, proxy: 10010, grpc: 10020, tm_rpc: 10030, tm_grpc: 10040, ipfs_swarm: 10050, ipfs_api: 10060, ipfs_gateway: 10070, web: 8310, moniker: "asset_chain0"]'
          genesis_config: asset_chain.toml
          moderator:
            sk: tk3pcIjpDRzeUGutXL7mjf52jNfA_kztlQnIgYnBStiY2X5pZlsLGwfiMgFA6a8qLhCEgMjGmEBjcFROew9TXw
            pk: mNl-aWZbCxsH4jIBQOmvKi4QhIDIxphAY3BUTnsPU18
            address: z1TpjUv5ZVVpY854GVk9W9Zfnb4HKqQzRSg
          forge_bin: /the/repo/path/to/forge/_build/staging/rel/forge/bin/forge
          forge_web_bin: /the/repo/path/to/forge-web/_build/staging/rel/forge_web/bin/forge_web
          tm_bin: /the/repo/path/to/forge/_build/tmp/darwin/tm/tendermint
          forge_root: /The/path/to/your/home/folder/.asset_chain
        - chain_id: app_chain
          validators:
            - '[address: "127.0.0.1", p2p: 10100, proxy: 10110, grpc: 10120, tm_rpc: 10130, tm_grpc: 10140, ipfs_swarm: 10150, ipfs_api: 10160, ipfs_gateway: 10170, web: 8410, moniker: "app_chain0"]'
          genesis_config: app_chain.toml
          moderator:
            sk: AbGCbsZRQuk4bJbtU4-1ZqJc41YDgvISZC2BDpC4pGdZS2ai83D8N-QM9p9_FBzsmMZD2o4HzmE6gLo6Lxqf2Q
            pk: WUtmovNw_DfkDPaffxQc7JjGQ9qOB85hOoC6Oi8an9k
            address: z1VFy8hB9ndynkWAAH9P1a2L5WaU7AvtKGy
          forge_bin: /the/repo/path/to/forge/_build/staging/rel/forge/bin/forge
          forge_web_bin: /the/repo/path/to/forge-web/_build/staging/rel/forge_web/bin/forge_web
          tm_bin: /the/repo/path/to/forge/_build/tmp/darwin/tm/tendermint
          forge_root: /The/path/to/your/home/folder/.app_chain
     ```
  4. `cp ./resources/app_chain.toml ~/.forge_patron/`
  5. `cp ./resources/asset_chain.toml ~/.forge_patron/`
  6. `forge-patron start`
  7. `make run`