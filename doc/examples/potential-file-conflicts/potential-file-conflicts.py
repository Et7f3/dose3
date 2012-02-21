#!/usr/bin/python 

# Copyright (C) 2012 Ralf Treinen <treinen@debian.org>
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

import argparse
import re
import os
import sys
import subprocess
import yaml

argparser=argparse.ArgumentParser(
  description="Find packages with potential file conflicts.")
argparser.add_argument('-c',dest='contentsfile',action='store',required=True,
                       help='set name of the Contents file.')
argparser.add_argument('-o',dest='outdir',action='store',required=True,
                       help='set name of the output directory')
argparser.add_argument('-r',dest='repositories',action='append',required=True,
                       help='add a debian repository (Packages) file')
arguments=argparser.parse_args()

outdir = arguments.outdir
if os.path.exists(outdir):
    raise('directory'+outdir+'already exists')
else:
    os.mkdir(arguments.outdir)

############################################################################
# read the contentsfile into a dictionary
############################################################################
cntsf=open(arguments.contentsfile,'r')
print 'Scanning the contents file ...',
sys.stdout.flush()
# Skip the preamble
while True:
    if re.match('FILE\s*LOCATION\s*',cntsf.readline()): break
# Lines start on a file, then a comma-separated list of packages. We are only
# interested in lines that contain at least two packages. Packages are given
# as area/section/packagename or section/packagename.
linefilter=re.compile('^(.*\S)\s+(\S*,\S*)\s*$')
filetable = {}
for line in cntsf:
    linematch=re.match(linefilter,line)
    if linematch:
        foundfile=linematch.group(1)
        # get the packages as a sorted list. Drop package prefix consisting
        # of (possibly area and) section.
        foundpackages=sorted(map(
                lambda s:s[1+s.rfind('/'):],
                re.split(',',linematch.group(2))))
        numberpackages=len(foundpackages)
        for pair in [ (foundpackages[i],foundpackages[j])
                      for i in range(numberpackages-1)
                      for j in range(i+1,numberpackages)]:
            if pair not in filetable:
                filetable[pair]=foundfile+'\n'
            else:
                filetable[pair] += foundfile+'\n'
cntsf.close()
print 'done.'

###########################################################################
# run debcheck
###########################################################################

invocation='/usr/bin/dose-debcheck --successes'
for repo in arguments.repositories:
    invocation += ' --bg=deb://' + repo

print 'Running dose-debcheck ...',
sys.stdout.flush()
debcheckproc=subprocess.Popen(invocation,shell=True,
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE)

# write the input file for debcheck, containing a pseudo-package for each
# of the pairs (pa,pb) that we found, depending on pa and on pb. We use
# bogus version constraint >=0 to enforce that this dependency can only be
# satisfied by a real package, not a virtual package.
#
# We pipe that input directly into the debcheck process. We use 
for packages,files in filetable.iteritems():
    pa=packages[0]
    pb=packages[1]
    debcheckproc.stdin.write('Package: '+pa+':'+pb+'\n')
    debcheckproc.stdin.write('Version: 1\nArchitecture: all\n')
    debcheckproc.stdin.write('Depends: '+pa+'(>=0), '+pb+'(>=0)\n\n')
debcheckproc.stdin.close()

# read the report generated by debcheck. Since we called debcheck with
# option --successes we get only stanzas that encode co-installable
# pairs of packages.
debreport = yaml.load(debcheckproc.stdout)
print 'done.'
if debreport['report'] is not None:
    os.chdir(outdir)
    for stanza in debreport['report'] :
        stanzamatch=re.match('^(.*):(.*)',stanza['package'])
        pa,pb=stanzamatch.group(1),stanzamatch.group(2)
        out=open(pa+':'+pb,'w')
        out.write(filetable[(pa,pb)])
        out.close()
