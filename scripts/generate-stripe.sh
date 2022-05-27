sudo rm -rf smos-stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedSmosStripeCode) smos-stripe-client
sudo chmod -R +rwx smos-stripe-client
