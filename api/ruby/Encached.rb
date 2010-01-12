# Encached Ruby Client Library v0.1
# Bogdan Golovchenko <fantomcsm@gmail.com>
# twitter: fantom_ua
# 10 january 2010

require 'socket'

class Encached
 attr_accessor :server, :port

def initialize(server,port)
  @server = server
  @port = port
  #puts "\r\n Connecting #{@server} on port: #{@port} \r\n"
  @s = TCPSocket.open(@server, @port)
end

def put(key, value)
  @s.print("PUT #{key.to_s.length} #{key} #{value.to_s.length}\r\n#{value}\r\n")

case @s.gets.chomp
  when 'SUCCESS'
    return true
  when 'FAILURE'
    return false
  when 'HIT'
    return 'HIT'
end
end

def get(key)
  @s.print("GET #{key.to_s.length} #{key}\r\n")
  response = @s.gets
  resp, len, data = response.split
  response = response.delete resp
  response = response.delete len   
  response_len = response.lstrip.rstrip.length
 
case resp
  when 'DATA'
  begin
if response_len >= len.to_i
else
  until response_len > len.to_i do
  response2 = @s.gets
  response_len = response_len + response2.length
  response = response + response2
end
end      
  return response.lstrip
  end
    when 'NODATA'
    return false
end
end

def remove(key)
  @s.print("REMOVE #{key.to_s.length} #{key}\r\n")

case @s.gets.chomp
  when 'SUCCESS'
    return true
  when 'FAILURE'
    return false
end
  end
    end

