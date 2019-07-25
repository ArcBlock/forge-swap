TOP_DIR=.
BUILD_DIR=$(TOP_DIR)/_build
REL_DIR=_build/releases
VERSION=$(strip $(shell cat version))
TARGETS=centos ubuntu darwin
APP_NAME=forge_swap
TEST_PATH=/tmp/.forge_swap_test/

dep:
	@echo "Install dependencies required for this repo..."
	@cd src; mix deps.get

$(TARGETS):
	@echo "Building the $@ release"
	@mkdir -p $(REL_DIR)
	@cd assets; npm install; npm run deploy
	@mix phx.digest
	@rm -rf _build/staging/rel/$(APP_NAME); MIX_ENV=staging mix release; tar zcf $(REL_DIR)/$(APP_NAME)_$@_amd64.tgz -C _build/staging/rel/$(APP_NAME) .

build:
	@echo "Building the software..."
	@rm -rf priv/gen
	@rm -rf _build/dev/lib/$(APP_NAME)
	@make format

rebuild:
	@rm -rf assets/node_modules
	@rm -rf priv/static
	@cd assets; yarn
	# @make upgrade
	@mix phx.digest
	@mix compile

format:
	@cd src; mix compile; mix format;

run:
	@echo "Running the software..."
	@cd src; FORGESWAP_CONFIG=../resources/test.toml iex -S mix phx.server

test:
	@echo "Running test suites..."
	@MIX_ENV=test make build
	@cd src; MIX_ENV=test FORGESWAP_CONFIG=../resources/test.toml mix test --trace $(TF)

dialyzer:
	mix dialyzer

# Test under integration topology
ti: patron-start
	@MIX_ENV=integration make build
	@cd src; MIX_ENV=integration FORGESWAP_CONFIG=../resources/test.toml mix test --trace test/forge_swap_web/integration/integration_test.exs
	@forge-patron stop

# Run under Integration topology
ri: patron-start
	@make run

patron-start:
	@rm -rf ~/.forge_patron
	@rm -rf $(TEST_PATH)
	@forge-patron init
	@cp ./resources/patron/*.toml ~/.forge_patron
	@cp ./resources/patron/*.yml ~/.forge_patron
	@mkdir -p $(TEST_PATH)
	@mkdir $(TEST_PATH)/forge; tar xzvf ./resources/patron/forge_darwin_amd64.tgz -C $(TEST_PATH)/forge
	@mkdir $(TEST_PATH)/forge_web; tar xzvf ./resources/patron/forge_web_darwin_amd64.tgz -C $(TEST_PATH)/forge_web
	@cp ./resources/patron/tendermint $(TEST_PATH)/tendermint
	@forge-patron start
	@echo "Waiting for patron to start"; sleep 10;


# upgrade:
# 	@cd assets; yarn install; yarn upgrade @arcblock/forge-web; cp node_modules/@arcblock/forge-web/build/index.html .; sed -i 's/\.\//\//g' index.html; npm run deploy;

include .makefiles/*.mk
