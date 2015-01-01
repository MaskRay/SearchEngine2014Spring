require 'nokogiri'
require 'find'
require 'uri'
require 'uri'

class Link
  def initialize
    @uri2id = {}
  end

  def get_id uri
    uri.query = nil
    uri.fragment = nil
    unless @uri2id[uri]
      id = @uri2id.size
      @uri2id[uri] = id
      puts "+ #{id} #{uri}"
    end
    @uri2id[uri]
  end

  def process_edge u, v
    puts "#{get_id(u)} #{get_id(v)}"
  end

  def run
    Find.find('/home/ray/crawl') {|path|
      next unless path =~ /tsinghua/
      uri = URI "http://#{URI.escape(path.sub(/.*?crawl\//, '')).gsub('[','%5B').gsub(']','%5D') }"
      #if uri.to_s.scan('tsinghua.edu.cn').size > 1
      #  puts uri
      #  exit
      #end
      if FileTest.directory? path
        #puts "dir #{path}"
      else
        if path =~ /\.(html?|asp|jsp|php)$/i
          page = Nokogiri::HTML open(path)
          anchor = page.css('a')
          anchor.each {|a|
            begin
              process_edge uri, URI.join(uri, a['href'])
            rescue
            end
          }
        end
      end
    }
  end
end

Link.new.run
