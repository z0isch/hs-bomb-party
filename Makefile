.PHONY: help
help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: ghcid-devel
ghcid-devel: ## Run the server in fast development mode. See DevelMain for details.
	ghcid \
	    --command "stack ghci hs-bomb-party:lib" \
	    --test "DevelMain.update"

.PHONY: push-image
push-image: ## Build the container image and push it as latest to docker
	docker build -t hs-bomb-party . --tag z0isch/hs-bomb-party:latest && docker push z0isch/hs-bomb-party:latest

.PHONY: watch-css
watch-css: 
	npx tailwindcss -i ./static/css/input.css -o ./static/css/output.css --minify --watch