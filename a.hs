esp :: Int -> String
esp 0 = []
esp a = ' ':esp(a-1)

addEspOnStr :: String -> Int -> String
addEspOnStr [] b = []
addEspOnStr (x:xa) b = if xa == [] then [x] else (x:[]) ++ (esp(b)) ++ (addEspOnStr xa b)

quantidade :: Int -> Int -> Int
quantidade 0 0 = 1
quantidade 0 b = nap 0 b
quantidade a b = (nap a b) + (quantidade (a-1) b)

nap :: Int -> Int -> Int
nap 0 b = 0
nap a b
 | (mod a 10) == b = 1 + (nap (div a 10) b)
 | otherwise = (nap (div a 10) b)
--
semum :: Int -> Int
semum 0 = 0
semum x
    | mod x 10 == 1 = semum (div x 10)
    | otherwise = (mod x 10) + 10*(semum (div x 10))

cheacaz :: Int -> Bool
cheacaz x
    | x < 10 = False
    | mod x 10 == 0 = True
    | otherwise = cheacaz (div x 10)

pegan :: Int -> [Int]
pegan 1 = []
pegan 0 = [0]
pegan x = if((semum (x)) == 0 && ((cheacaz x) == False)) then [] else (semum(x)):[]

limpaum :: [Int] -> [Int]
limpaum [] = []
limpaum (x:xa) = if (pegan x) == [] then limpaum xa else head (pegan x):(limpaum xa)
--

len [] = 0
len (x:xa) = 1 + len (xa)

get :: String -> Int -> Char
get (x:xa) 1 = x
get (x:xa) b = get xa (b-1)

inv :: String -> Int -> String
inv xa 0 = []
inv [] b = []
inv xa b = (get xa b):(inv xa (b-1))

comp :: String -> String -> Bool
comp [] [] = True
comp (x:xa) (y:ya)
    | x == y = comp xa ya
    | otherwise = False
    
isPalindromo :: String -> Bool
isPalindromo xa = comp xa (inv xa (len xa))


testaki :: [Int] -> [Int] -> Bool
testaki a b 
    | a == [] = True
    | otherwise = False

trad '1' = 1
trad '0' = 0

btoi :: String -> Int
btoi [] = 0
btoi (x:xa) = ((2^len(xa)) * trad(x)) + btoi xa


metade :: [Int] -> [[Int]]
metade [] = [[]]
metade x = (cut x (div (len x) 2)) : (mtail x (div (len x) 2)) :[]

cut :: [Int] -> Int -> [Int]
cut x 0 = []
cut (x:xa) tam = x:(cut xa (tam-1))

mtail :: [Int] -> Int -> [Int]
mtail x 0 = x
mtail (x:xa) tam = (mtail xa (tam-1))

calr :: (Float,Float,Float) -> [Float]
calr (a,b,c)
    | a == 0 = []
    | (b^2) -(4*a*c) < 0 = []
    | (b^2) - (4*a*c) == 0 = (((-b)+sqrt(b^2 - (4*a*c)))/(2*a)):[] 
    | otherwise = (((-b)+sqrt(b^2 - (4*a*c)))/(2*a)): (((-b)-sqrt(b^2 - (4*a*c)))/(2*a)):[]
--quick :: [Int] -> [Int]
--quick xs = [x | (x:xa) <- xs, (head xa) > x]

type Pes = String
type Liv = String
type Db = [(Pes,Liv)]

banco = [("Rodrigo","Plc"),("Afonso","SD"),("Zenio","Zoeira"),("Kinho","Gamb"),("Kinho","pseudonimos para fugir do EB"),("Victor", "Gambreverse"),("Zilde","Agua"),("Pedro", "Selva"),("Xmemes","Alagoas")]
livros:: Db -> Pes -> [Liv]
livros db pes = [a | (x,a) <- db, x == pes]

emprestimos :: Db -> Liv -> [Pes]
emprestimos db liv = [a | (a,x) <- db, x == liv]

emprestado :: Db -> Liv -> Bool
emprestado db liv = ((length([a | (x,a) <- db, a == liv])) > 0)

qtdemprestimos :: Db -> Pes -> Int
qtdemprestimos db pes = length([a| (a,x) <- db, a == pes])

emprestar :: Db -> Pes -> Liv -> Db
emprestar [] pes liv = (pes,liv):[]
emprestar db pes liv = db ++ [(pes,liv)]

quickluck [] = []
quickluck (a:xs) = quickluck([x | x <- (a:xs), x < a]) ++ a:(quickluck ([x | x <- (a:xs), x > a])) 
