# require File.dirname(__FILE__) + "/fix-xfig-fonts.rb"

desc "Crunch all xfigs to eps"
task :epspictures

desc "Crunch all xfigs to pdf"
task :pdfpictures

def latex filename
  desc "LaTeX #{filename}.tex"
  file "#{filename}.dvi" => ["#{filename}.tex", :epspictures] do
    sh "latex #{filename}.tex"
  end
  task :latex => "#{filename}.dvi"
end

def pdflatex filename
  desc "LaTeX #{filename}.tex to a PDF"
  file "#{filename}.pdf" => ["#{filename}.tex", :pdfpictures] do
    sh "pdflatex #{filename}.tex"
  end
  task :pdflatex => "#{filename}.pdf"
end

def autobib filename
  file "#{filename}.aux" => :pdflatex
  file "#{filename}-auto.bib" => "#{filename}.aux" do
    bibdump = "#{ENV['HOME']}/work/papers/bibdump.rb"
    if File.executable?(bibdump)
      sh "#{bibdump} #{filename}"
    else
      puts "Can't find the bibliography dumping program, ignoring"
      touch "#{filename}-auto.bib"
    end
  end
  file "#{filename}.bbl" => ["#{filename}-auto.bib"] do
    sh "bibtex #{filename}"
  end
  desc "Regenerate the bibliography data"
  task :bibtex => "#{filename}.bbl"
end

## Xfig integration

# eps or pdf
# latex string processing (need figure size) or not
# layer selection
# layered animations
# - named layers?

def figure outfile, infile=outfile
  process_fig outfile, infile, "eps", ""
  process_fig outfile, infile, "pdf", ""
end

def fig_layer outfile, layers, infile=outfile
  process_fig outfile, infile, "eps", "-K -D #{layers}"
  process_fig outfile, infile, "pdf", "-K -D #{layers}"
end

def fig_animation outfile, layers_list, infile=outfile
  layers_list.each_index do |i|
    fig_layer "#{outfile}-#{i}", layers_list[i], infile
  end
end

def special_figure outfile, size=12, infile=outfile
  process_fig outfile, infile, "pstex",  "-Z #{size}"
  process_fig outfile, infile, "pdftex", "-Z #{size}"
end

def special_fig_layer outfile, layers, infile=outfile, size=12
  process_fig outfile, infile, "pstex",  "-K -D #{layers} -Z #{size}"
  process_fig outfile, infile, "pdftex", "-K -D #{layers} -Z #{size}"
end

def special_fig_animation outfile, layers_list, infile=outfile, size=12
  layers_list.each_index do |i|
    special_fig_layer "#{outfile}-#{i}", layers_list[i], infile, size
  end
end

def process_fig outfile, infile, format, options
  ensure_legal_fig_format format
  full_out_name = "#{outfile}.#{format}"
  desc "Produce #{full_out_name}"
  file full_out_name => "#{infile}.fig" do
    sh "fig2dev -L #{format} #{options} #{infile}.fig #{full_out_name}"
  end
  task pictures_task(format) => full_out_name
  if fig_latex_strings_wanted? format
    file "#{full_out_name}_t" => full_out_name do
      sh "fig2dev -L #{format}_t #{options} -F -p #{full_out_name} #{infile}.fig #{full_out_name}_t"
      fix_font_size outfile, format
    end
    if format == "pdftex"
      task "#{full_out_name}_t" => "#{outfile}.pstex"
    end
    task pictures_task(format) => "#{full_out_name}_t"
  end
end

def legal_fig_formats
  ["eps", "pdf", "pstex", "pdftex"]
end

def ensure_legal_fig_format format
  if legal_fig_formats.member? format
    # OK
  else
    raise "Illegal format #{format} for figure output."
  end
end

def pictures_task format
  case format
  when "eps"   : :epspictures
  when "pstex" : :epspictures
  when "pdf"   : :pdfpictures
  when "pdftex": :pdfpictures
  end
end

def fig_latex_strings_wanted? format
  ["pstex", "pdftex"].member? format
end
