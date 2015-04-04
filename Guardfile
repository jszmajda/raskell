# A sample Guardfile
# More info at https://github.com/guard/guard#readme

## Uncomment and set this to only include directories you want to watch
# directories %w(app lib config test spec features)

## Uncomment to clear the screen before every task
# clearing :on

## Guard internally checks for changes in the Guardfile and exits.
## If you want Guard to automatically start up again, run guard in a
## shell loop, e.g.:
##
##  $ while bundle exec guard; do echo "Restarting Guard..."; done
##
## Note: if you are using the `directories` clause above and you are not
## watching the project directory ('.'), then you will want to move
## the Guardfile to a watched dir and symlink it back, e.g.
#
#  $ mkdir config
#  $ mv Guardfile config/
#  $ ln -s config/Guardfile .
#
# and, you'll have to watch "config/Guardfile" instead of "Guardfile"

tail_opts = "--color"
hs_opts = '-package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.4-packages.conf.d -isrc -itest'
run_hs = "runhaskell #{hs_opts}"
guard :shell do
  watch(%r{^src/(.*)(.hs)}) do |m|
    fname = m[1]
    if fname =~ /ASTNodes$/
      fname = 'Raskell/Parser/RubyParser'
    end
    puts "---------------------------------------------------"
    `env #{run_hs} #{fname.gsub(/\//,'.')}Spec #{tail_opts}`
  end
  watch(%r{^test/(.*)(.hs)}) do |m|
    fname = m[1]
    puts "---------------------------------------------------"
    `env #{run_hs} #{fname.gsub(/\//,'.')} #{tail_opts}`
  end
end
