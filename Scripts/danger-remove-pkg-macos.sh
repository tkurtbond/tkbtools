#$ pkgutil --pkg-info the-package-name.pkg # check the location
(cd / # assuming the package is rooted at /...
     pkgutil --only-files --files $1 | tr '\n' '\0' | xargs -n 1 -0 sudo rm -f
     pkgutil --only-dirs --files $1 | /usr/bin/tail -r | tr '\n' '\0' | xargs -n 1 -0 sudo rmdir)
