#!/bin/bash

USAGE="usage: ./make_keter_bundle [path to binary] [env: stage|prod]"

if [ "$#" -ne 2 ]; then
    echo $USAGE
    exit 1
fi

if [ $2 == "stage" ]; then
  export ENV_HOST="beta.taplike.com"
  echo "building for env: stage host:beta.taplike.com"
elif [ $2 == "prod" ] 
then
  export ENV_HOST="taplike.com"
  echo "building for env: production host: taplike.com"
else
  echo $USAGE
  exit 1
fi

echo "making bundle for ${ENV_HOST}"
stack build
strip $1
rm -rf static/tmp
tar czfv taplike.keter $1 config static