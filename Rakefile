task :clean do
  %w(.bin .bci .com .ext ~).each do |extension|
    sh "find . -name \"*#{extension}\" -delete"
  end
end

excludes = %w(*.bin *.com *.bci *.ext *~ *.svn selectors explorations .gitignore todo.txt partial-composition.* reference.* revised-html)

task :release => [:workbook, :doc] do
  sh "cd #{File.dirname(__FILE__)}; " + %Q{tar --create --verbose --file ../propagator.tar --directory .. --transform "s/prop/propagator/" --exclude=} +"\"" + excludes.join("\" --exclude=\"") + "\" prop/"
end

def files
  ["scheduler",
   "metadata",
   "merge-effects",
   "cells",
   "cell-sugar",
   "propagators",
   "application",
   "sugar",
   "generic-definitions",
   "compound-data",
   "physical-closures",
   "standard-propagators",
   "carrying-cells",

    "intervals",
    "premises",
    "supported-values",
    "truth-maintenance",
    "contradictions",
    "search",
    "amb-utils",

    "example-networks"].map do |base|
  "core/#{base}.scm"
  end
end

task :workbook do
  sh "enscript -M letter -fCourier-Bold12 -o workbook.ps --file-align=2 #{files.join(" ")}"
end

task :doc do
  sh "cd #{File.dirname(__FILE__)}/doc; rake doc"
end

task :push => :release do
  sh "cp ../propagator.tar /afs/csail.mit.edu/group/mac/www/data/users/gjs/propagators/"
  sh "cp doc/revised-html.html /afs/csail.mit.edu/group/mac/www/data/users/gjs/propagators/"
  sh "cp doc/revised-html.html /afs/csail.mit.edu/group/mac/www/data/users/gjs/propagators/index.html"
end
