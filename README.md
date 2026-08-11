# `oci-distribution-api-hs`

Small library implementing the [OCI Distribution Spec](https://github.com/opencontainers/distribution-spec) API.

## Endpoints implemented

Only a subset of endpoints are currently implemented. See
[`OpenContainerImage.Registry`](./src/OpenContainerImage/Registry.hs)

- end-2 `getImageManifest`
- end-3 `withBlobFromDigest`

## Minimal example

See example program [`oci-read-one-layer`](./example/oci-read-one-layer.hs).

Connecting the example program to an Amazon ECR private registry can look like:

``` sh
export AWS_REGION=us-east-2
export AWS_ACCOUNT_ID=123123123123
export ECR_PASS=$(aws ecr get-login-password --region "$AWS_REGION")
stack run oci-read-one-layer -- \
  --baseUrl "https://AWS:${ECR_PASS}@${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com" \
  --manifest YOUR_NAMIFEST_NAME_HERE \
  --ref YOUR_MANIFEST_REF_HERE \
  --layer YOUR_LAYER_TITLE_HERE
```

This performs the requisite auth flow for requests.
