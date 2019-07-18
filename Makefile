TOP_DIR=.
BUILD_DIR=$(TOP_DIR)/_build
REL_DIR=_build/releases
VERSION=$(strip $(shell cat version))
TARGETS=centos ubuntu darwin
APP_NAME=forge_swap

dep:
	@echo "Install dependencies required for this repo..."
	@mix deps.get

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
	@cd src; iex -S mix phx.server

test:
	@echo "Running test suites..."
	@MIX_ENV=test make build
	@cd src; MIX_ENV=test FORGESWAP_CONFIG=../resources/test.toml mix test --trace $(TF)

dialyzer:
	mix dialyzer

# upgrade:
# 	@cd assets; yarn install; yarn upgrade @arcblock/forge-web; cp node_modules/@arcblock/forge-web/build/index.html .; sed -i 's/\.\//\//g' index.html; npm run deploy;

include .makefiles/*.mk
