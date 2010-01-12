require 'Encached'

cache = Encached.new('127.0.0.1', 2332)

cache.put('hello',"Hello World")
#cache.put('email123', "fantomcsm@gmail.com")

#puts cache.get('email123')
puts cache.get('hello')

cache.remove('hello')
#cache.remove('email123')
