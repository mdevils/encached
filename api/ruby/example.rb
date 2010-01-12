require 'Encached'

cache = Encached.new('127.0.0.1', 2332)


cache.remove('hello')
cache.remove('email123')
cache.remove('DATA 123')

cache.put('hello',"Hello World")
cache.put('email123', "fantomcsm@gmail.com")
cache.put('DATA',"DATA 12345")

puts cache.get('email123')
puts cache.get('hello')
puts cache.get('DATA')
