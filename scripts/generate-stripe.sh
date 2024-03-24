nix build .#generatedSmosStripeCode

rm -rf smos-stripe-client
mkdir -p smos-stripe-client
cp -R result/* smos-stripe-client
chmod -R 764 smos-stripe-client
