RELEASE_VERSION=v$(VERSION)
GIT_VERSION="$(strip $(shell git rev-parse --short HEAD))"
GIT_BRANCH=$(strip $(shell git symbolic-ref --short HEAD))

release:
	@git config --local user.name "Peiling Ding"
	@git config --local user.email "dingpl716@gmail.com"
	@git tag -a $(RELEASE_VERSION) -m "Release $(RELEASE_VERSION). Revision is: $(GIT_VERSION)" | true
	@git push origin $(RELEASE_VERSION) | true

delete-release:
	@echo "Delete a release on $(RELEASE_VERSION)"
	@git tag -d $(RELEASE_VERSION) || true
	@git push -f -d origin $(RELEASE_VERSION) || true

bump-version:
	@echo "Bump version..."
	@.makefiles/bump_version.sh

create-pr:
	@echo "Creating pull request..."
	@git push origin $(GIT_BRANCH)
	@hub pull-request

browse-pr:
	@hub browse -- pulls
