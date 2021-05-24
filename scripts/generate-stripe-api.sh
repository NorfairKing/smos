export PATH="$HOME/.local/bin:$PATH"

specRepo=${1:-stripe-api/stripe-spec}
outputDir=${2:-stripe-api/stripe-client}
openapi3-code-generator-exe "$specRepo/openapi/spec3.yaml" \
  --property-type-suffix="'" \
  --module-name "StripeClient" \
  --convert-to-camel-case \
  --package-name "stripe-client" \
  --output-dir "$outputDir" \
  --force \
  \
  --omit-additional-operation-functions \
  --operation-to-generate "GetEvents" \
  --operation-to-generate "GetPricesPrice" \
  --operation-to-generate "PostCustomers" \
  --operation-to-generate "PostCheckoutSessions" \
  --white-listed-schema event \
  --white-listed-schema checkout.session \
  --white-listed-schema notification_event_data \
  --white-listed-schema invoice \
  --white-listed-schema subscription \
  --white-listed-schema price \
  --white-listed-schema customer

tree $outputDir/src/StripeClient/

