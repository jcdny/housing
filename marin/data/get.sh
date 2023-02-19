#!/bin/env bash
# See https://gisopendata.marincounty.org/
#
curl 'https://data.marincounty.org/api/geospatial/aqx8-43z2?method=export&format=GeoJSON' | bzip2 > marin-geojson.json.bz2
