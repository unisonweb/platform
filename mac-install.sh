mkdir -p unisonlanguage
curl -# -L https://github.com/unisonweb/unison/releases/download/latest-macOS/unison-macOS.tar.gz --output unisonlanguage/ucm.tar.gz
tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage

echo ""
echo "All done!"
echo ""
echo "\x1b[1mRun ./unisonlanguage/ucm to start Unison.\x1b[0m" 
echo ""
echo "If you like, add the \x1b[4m./unisonlanguage\x1b[0m directory to your path"
echo "or make a symbolic link to \x1b[4m./unisonlanguage/ucm\x1b[0m somewhere on"
echo "your path."
