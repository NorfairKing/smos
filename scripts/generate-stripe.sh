sudo rm -rf smos-stripe-client
nix build .#generatedSmosStripeCode
cp -rHL result smos-stripe-client
sudo chmod -R +rwx smos-stripe-client
