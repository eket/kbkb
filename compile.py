import requests

r = requests.post('http://elm-lang.org/compile', {'input': open('a.elm').read()})
open('a.html', 'w').write(r.content.replace('/elm-runtime.js', 'https://elm-lang.org/elm-runtime.js'))
