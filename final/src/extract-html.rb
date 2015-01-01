require 'nokogiri'
require 'pathname'
require 'find'

class Link
  def initialize
  end

  def run
    cnt = 0
    Find.find('mirror') {|path|
    #Find.find('/tmp/mirror') {|path|
      next unless path =~ /tsinghua/
      html_path = Pathname path.sub('mirror', 'html')
      if FileTest.directory? path
      else
        if path =~ /\.(html?)$/i
          s = ''
          page = Nokogiri::HTML open(path)
          title = page.xpath('//title[text()]')
          if title.empty?
            title = ''
          else
            title = title[0].text.gsub "\n", ' '
          end
          page.xpath(['a','span','li','div','p','td','title','h1','h2','h3','h4'].map {|e| "//#{e}/text()" }.join('|')).each {|el|
            s << ' ' unless s.empty?
            s << el.text
          }

          html_path.parent.mkpath
          File.open(html_path, 'w') {|h|
            h.write "#{title}\n"
            h.write s
          }
          cnt += 1
          if cnt % 100 == 0
            puts "+ processed #{cnt} html"
          end
        end
      end
    }
  end
end

Link.new.run
