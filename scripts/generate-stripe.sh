sudo rm -rf stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedStripeCode) stripe-client
sudo chmod -R +rwx stripe-client
