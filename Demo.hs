import Lenses

myaddr :: Address
myaddr = A "Champs-Elysees" "Paris" "75000"

me :: Person
me = P "Xavier" myaddr 100000

main :: IO ()
main =
  print $ salary me
