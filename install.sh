#!/bin/bash

add-apt-repository universe ;
apt-get update ;
apt-get install nodejs ;
apt-get install cabal-install ;
nodejs -v ; 
cabal -V ;
