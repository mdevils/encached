# Encached Ruby Client Library v0.1
# Bogdan Golovchenko <fantomcsm@gmail.com>
# twitter: fantom_ua
# 13 january 2010

require 'socket'

class Encached
  attr_accessor :server, :port

def initialize(server,port)
  @server = server
  @port = port
  @s = TCPSocket.open(@server, @port)
end

def put(key, value)
@s.print("PUT #{key.to_s.length} #{key.to_s} #{value.to_s.length}\r\n#{value.to_s}\r\n")

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
  @s.print("GET #{key.to_s.length} #{key.to_s}\r\n")
  response = @s.gets.to_s
  resp, len, data = response.split
  response_len = response.lstrip.rstrip.length
 
case resp
  when 'DATA'
  begin
    response[0..3] = ''
    response[0..len.length.to_i] = ''
    
  if response_len >= len.to_i
  else
    len_wait = len.to_i
  until response_len > len_wait do
      response2 = @s.gets.to_s
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

