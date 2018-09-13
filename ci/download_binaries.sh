set -e

if [ -z "$TRAVIS_TAG" ]; then
    echo "TRAVIS_TAG variable empty, will not download binaries"
    exit 0
fi

BASE_URL=https://github.com/astrada/ppx_bs_css/releases/download/$TRAVIS_TAG/ppx_bs_css-

mkdir -p bin

for platform in linux-x64 darwin-x64 win-x64 win-x86; do
    (cd bin && curl -fOL $BASE_URL$platform.exe)
done
