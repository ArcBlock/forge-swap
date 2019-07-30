TOP_DIR=.
BUILD_DIR=$(TOP_DIR)/_build
REL_DIR=_build/releases
VERSION=$(strip $(shell cat version))
TARGETS=centos ubuntu darwin
APP_NAME=forge_swap
TEST_PATH=/tmp/.forge_swap_test/
FORGE_VERSION=$(strip $(shell cat .forge_version))

tt:
	@echo $(FORGE_VERSION)

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
	@rm -rf _build/dev/lib/$(APP_NAME)
	@make format

rebuild:
	@rm -rf src/assets/node_modules
	@rm -rf src/priv/static
	@cd src/assets; npm install; npm run deploy
	# cd assets && npm install && node node_modules/webpack/bin/webpack.js --mode development
	@cd src; mix phx.digest
	@make build

format:
	@cd src; mix compile; mix format;

run:
	@echo "Running the software..."
	@cd src; FORGESWAP_CONFIG=../resources/test.toml iex -S mix phx.server

test:
	@echo "Running test suites..."
	@MIX_ENV=test make build
	@cd src; MIX_ENV=test FORGESWAP_CONFIG=../resources/test.toml mix test --trace --exclude integration $(TF)

dialyzer:
	mix dialyzer

# Test under integration topology
test-all: init-forge start-patron
	@echo "Waiting for patron to start"; sleep 10;
	@MIX_ENV=test make build
	@cd src; MIX_ENV=test FORGESWAP_CONFIG=../resources/test.toml mix test --trace $(TF)

# Run under Integration topology
ri: patron-start
	@make run

init-forge:
	@echo "Installing forge chain"
	@forge install $(FORGE_VERSION) --silent
	@ls -al ~/.forge_cli/

start-patron:
	@rm -rf ~/.forge_patron
	@rm -rf $(TEST_PATH)
	@forge-patron init
	@cp ./resources/patron/*.toml ~/.forge_patron
	@cp ./resources/patron/*.yml ~/.forge_patron
	@forge-patron start -v $(FORGE_VERSION)

# upgrade:
# 	@cd assets; yarn install; yarn upgrade @arcblock/forge-web; cp node_modules/@arcblock/forge-web/build/index.html .; sed -i 's/\.\//\//g' index.html; npm run deploy;

include .makefiles/*.mk
