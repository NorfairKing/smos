# Smos data

The `Smos.Data` module specifies the current data format version as the `currentDataVersion` constant.

## Upgrading the data format

When upgrading the data format, follow the following procedure:

1. Increment `newestParsableDataVersion`.
2. Test that the new format is parse-able, in `test/Smos/Data/CompatibilitySpec.hs`.
3. Release Smos with forward compatibility.
4. Increment `currentDataVersion` to match `newestParsableDataVersion`.
5. Make sure to update the golden tests in `test/Smos/Data/CompatibilitySpec.hs`.
   They should fail so this should be clear.
6. Make sure that there is a still a scenario test that makes sure that a future version beyond `newestParsableDataVersion` fails to parse with a nice error message.
7. Release Smos with backward compatibility.
8. Optional: Increment `oldestParsableDataVersion` if you no longer wish to support the old version. You can then also remove the corresponding scenario tests in `test_resources/compatibility`.
