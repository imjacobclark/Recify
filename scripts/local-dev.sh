echo "Recify will listen on http://localhost:3000 and request callbacks to http://localhost:3000"

export PORT=3000
export fqdn="http://localhost:3000"

if test -z "$bearer" 
then
      echo "\$Bearer is not set, see the README, exiting..."
      exit
fi

if test -z "$clientID" 
then
      echo "\$ClientID is not set, see the README, exiting..."
      exit
fi

echo "Environment checks complete, grabbing dependencies..."

cabal install

echo "Got dependencies, compiling..."

stack build 

echo "Compilation complete, starting..."

stack exec recify-exe