set -ex
hub release create \
  --message "$(date +%F) Release" \
  -t release \
  "$(date +%F)" # The tag
